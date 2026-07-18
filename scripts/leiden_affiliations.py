#!/usr/bin/env python3
"""Extract the TU9 institution-consolidation mapping from the CWTS Leiden
Ranking Open Edition and write data-raw/leiden_affiliations.csv.

The Leiden Ranking merges affiliated organizations into a "core" university with
a relation type and weight:

  component   (weight 1)      counted fully inside the university (e.g. the
                             university hospital, integrated institutes)
  joint       (weight <1)     fractionally shared with another university
  associated  (no weight)     listed but NOT counted into the university

This is the same consolidation question our OpenAlex selection handles by hand;
having Leiden's authoritative mapping lets us build a "university incl. its
component affiliates" view for the OA metrics.

Only three small tables are needed. Rather than download the 2.2 GB data archive,
we read just those members out of the remote zip with HTTP range requests
(pure standard library -- no third-party dependency).

For each component/joint affiliate we also resolve its OpenAlex institution id
(via ror: lookup) so the R pipeline can OR the members together in a
corresponding_institution_ids filter without re-resolving at run time.

Re-run when a new Leiden Ranking Open Edition *Data* record appears: bump
LEIDEN_ZIP_URL. Honours OPENALEX_MAILTO and OPENALEX_API_KEY from the
environment (see the `secret` file).

Usage:  python3 scripts/leiden_affiliations.py
"""

import csv
import io
import os
import sys
import json
import time
import urllib.parse
import urllib.request
import zipfile

# CWTS Leiden Ranking Open Edition 2025 - Data (Zenodo 17471989, CC0;
# OpenAlex snapshot 2025-08). Bump to the newest *Data* record when released
# (note: some editions nest the TSVs under a top-level directory, which
# read_members() handles by matching on the file basename).
LEIDEN_ZIP_URL = (
    "https://zenodo.org/records/17471989/files/"
    "cwts_leiden_ranking_open_edition_2025.zip?download=1"
)
LEIDEN_SOURCE = "CWTS Leiden Ranking Open Edition 2025 - Data (Zenodo 17471989, CC0)"

MEMBERS = ("university.tsv",
           "university_affiliated_organization.tsv",
           "affiliated_organization.tsv")

# Relation types whose OpenAlex id we resolve (the ones a consolidated view uses).
RESOLVE_RELATIONS = ("component", "joint")

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
INSTITUTIONS_CSV = os.path.join(HERE, "data-raw", "institutions.csv")
OUT_CSV = os.path.join(HERE, "data-raw", "leiden_affiliations.csv")


class HTTPRangeFile(io.RawIOBase):
    """A minimal seekable, read-only file over HTTP range requests, enough for
    zipfile to read the central directory and extract individual members."""

    def __init__(self, url):
        self.url = url
        self.pos = 0
        req = urllib.request.Request(url, headers={"Range": "bytes=0-0"})
        with urllib.request.urlopen(req, timeout=60) as r:
            cr = r.headers.get("Content-Range", "")
            if "/" in cr:
                self.size = int(cr.rsplit("/", 1)[1])
            else:
                self.size = int(r.headers.get("Content-Length", 0))
        if not self.size:
            raise RuntimeError("could not determine remote file size")

    def seekable(self):
        return True

    def readable(self):
        return True

    def tell(self):
        return self.pos

    def seek(self, offset, whence=io.SEEK_SET):
        if whence == io.SEEK_SET:
            self.pos = offset
        elif whence == io.SEEK_CUR:
            self.pos += offset
        elif whence == io.SEEK_END:
            self.pos = self.size + offset
        return self.pos

    def read(self, n=-1):
        if n is None or n < 0:
            n = self.size - self.pos
        if n <= 0 or self.pos >= self.size:
            return b""
        end = min(self.pos + n, self.size) - 1
        req = urllib.request.Request(
            self.url, headers={"Range": f"bytes={self.pos}-{end}"})
        attempts = 6
        for attempt in range(attempts):
            try:
                with urllib.request.urlopen(req, timeout=120) as r:
                    data = r.read()
                break
            except Exception as e:  # noqa: BLE001 - transient network / 429, retry
                if attempt == attempts - 1:
                    raise
                # Honour Retry-After on 429/503; otherwise exponential backoff.
                wait = 4.0 * (2 ** attempt)
                ra = getattr(e, "headers", None)
                if ra is not None and ra.get("Retry-After"):
                    try:
                        wait = max(wait, float(ra.get("Retry-After")))
                    except ValueError:
                        pass
                sys.stderr.write("  range read retry in %.0fs (%s)\n" % (wait, e))
                time.sleep(wait)
        self.pos += len(data)
        return data

    # zipfile calls readinto on the underlying buffer
    def readinto(self, b):
        data = self.read(len(b))
        b[: len(data)] = data
        return len(data)


def read_members(url):
    """Return {member_name: [dict rows]} for the three mapping TSVs. Members are
    matched on their basename, so a top-level directory prefix in the archive
    (as in the 2025 edition) is handled transparently."""
    tables = {}
    with zipfile.ZipFile(HTTPRangeFile(url)) as z:
        by_base = {os.path.basename(i.filename): i.filename for i in z.infolist()}
        for name in MEMBERS:
            path = by_base.get(name)
            if path is None:
                raise RuntimeError("member %s not found in archive" % name)
            with z.open(path) as fh:
                text = io.TextIOWrapper(fh, encoding="utf-8")
                tables[name] = list(csv.DictReader(text, delimiter="\t"))
    return tables


def read_our_institutions():
    with open(INSTITUTIONS_CSV, encoding="utf-8") as fh:
        rows = list(csv.DictReader(fh))
    by_ror = {r["ror_id"]: r for r in rows}
    return rows, by_ror


def openalex_id_by_ror(ror, mailto, api_key, cache):
    if ror in cache:
        return cache[ror]
    params = {"select": "id", "mailto": mailto}
    if api_key:
        params["api_key"] = api_key
    url = "https://api.openalex.org/institutions/ror:%s?%s" % (
        ror, urllib.parse.urlencode(params))
    oid = ""
    try:
        with urllib.request.urlopen(url, timeout=60) as r:
            obj = json.load(r)
        full = obj.get("id", "") or ""
        oid = full.rsplit("/", 1)[1] if full else ""
    except Exception as e:  # noqa: BLE001 - best effort; blank on miss
        sys.stderr.write("  no OpenAlex id for ror %s: %s\n" % (ror, e))
    cache[ror] = oid
    time.sleep(0.1)
    return oid


def main():
    mailto = os.environ.get("OPENALEX_MAILTO", "open-access@slub-dresden.de")
    api_key = os.environ.get("OPENALEX_API_KEY", "")

    print("Reading Leiden mapping tables from the remote zip ...")
    tables = read_members(LEIDEN_ZIP_URL)

    # Column names differ between editions; look each value up by the first key
    # present. The 2025 edition prefixes column names and, helpfully, ships the
    # OpenAlex institution id directly (as a bare numeric, e.g. "4210120689").
    def g(row, *keys):
        for k in keys:
            v = row.get(k)
            if v not in (None, ""):
                return v
        return ""

    def as_oaid(raw):
        raw = (raw or "").strip()
        if not raw:
            return ""
        return raw if raw.upper().startswith("I") else "I" + raw

    uni = {g(r, "university_ror_id", "ror_id"): r for r in tables["university.tsv"]}
    uao = tables["university_affiliated_organization.tsv"]
    aff_name = {g(r, "affiliated_organization_ror_id", "ror_id"):
                g(r, "affiliated_organization_ror_name", "ror_name")
                for r in tables["affiliated_organization.tsv"]}

    our_rows, our_by_ror = read_our_institutions()
    # our university-type institutions that Leiden treats as core universities
    cores = [r for r in our_rows
             if r.get("type") == "university" and r["ror_id"] in uni]
    print("TU9 core universities matched in Leiden: %d/%d" % (
        len(cores), sum(r.get("type") == "university" for r in our_rows)))

    rank = {"component": 0, "joint": 1, "associated": 2}
    cache = {}
    out_rows = []
    for core in sorted(cores, key=lambda r: r["slug"]):
        ror = core["ror_id"]
        recs = [r for r in uao if g(r, "university_ror_id") == ror]
        recs.sort(key=lambda r: (
            rank.get(r["relation_type"], 9),
            -(float(g(r, "affiliated_organization_weight") or 0)),
            g(r, "affiliated_organization_ror_id")))
        for r in recs:
            a = g(r, "affiliated_organization_ror_id")
            rel = r["relation_type"]
            name = aff_name.get(a) or g(uni.get(a, {}), "university_ror_name", "ror_name")
            # Prefer the OpenAlex id the mapping now ships; else an id we track;
            # else resolve via OpenAlex (only for the relations a view uses).
            oid = as_oaid(g(r, "affiliated_organization_openalex_institution_id"))
            if not oid and rel in RESOLVE_RELATIONS:
                if a in our_by_ror:
                    oid = our_by_ror[a]["openalex_id"]
                else:
                    oid = openalex_id_by_ror(a, mailto, api_key, cache)
            out_rows.append({
                "tu9_slug": core["slug"],
                "university_ror_id": ror,
                "university_name": core["name"],
                "relation_type": rel,
                "weight": r["affiliated_organization_weight"],
                "affiliated_ror_id": a,
                "affiliated_name": name,
                "affiliated_openalex_id": oid,
                "tracked_slug": our_by_ror[a]["slug"] if a in our_by_ror else "",
            })

    fields = ["tu9_slug", "university_ror_id", "university_name",
              "relation_type", "weight", "affiliated_ror_id",
              "affiliated_name", "affiliated_openalex_id", "tracked_slug"]
    with open(OUT_CSV, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=fields)
        w.writeheader()
        w.writerows(out_rows)

    n_comp = sum(r["relation_type"] == "component" for r in out_rows)
    n_res = sum(bool(r["affiliated_openalex_id"]) for r in out_rows)
    print("Wrote %s (%d rows; %d components; %d with OpenAlex id)." % (
        OUT_CSV, len(out_rows), n_comp, n_res))
    print("Source: %s" % LEIDEN_SOURCE)


if __name__ == "__main__":
    main()
