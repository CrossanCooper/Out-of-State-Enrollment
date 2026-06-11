import sys
import csv
import gzip
import json
import tempfile
import unittest
from pathlib import Path


CODE_DIR = Path(__file__).resolve().parents[1]
if str(CODE_DIR) not in sys.path:
    sys.path.insert(0, str(CODE_DIR))

from build_acs_town_covariates import (  # noqa: E402
    TownKey,
    collect_acs_api_estimates,
    collect_acs_estimates,
    match_places,
    parse_number,
    safe_ratio,
    strip_place_suffix,
)


class AcsTownCovariatesTest(unittest.TestCase):
    def test_strip_place_suffix_removes_legal_suffixes(self):
        self.assertEqual(strip_place_suffix("Tuscaloosa city, Alabama"), "TUSCALOOSA")
        self.assertEqual(strip_place_suffix("Mountain Brook city, Alabama"), "MOUNTAIN BROOK")
        self.assertEqual(strip_place_suffix("Lake Purdy CDP, Alabama"), "LAKE PURDY")
        self.assertEqual(strip_place_suffix("Yazoo City city, Mississippi"), "YAZOO CITY")

    def test_parse_number_treats_sentinel_values_as_missing(self):
        self.assertEqual(parse_number("12,345"), 12345)
        self.assertEqual(parse_number("42.5"), 42.5)
        self.assertIsNone(parse_number(""))
        self.assertIsNone(parse_number("(X)"))
        self.assertIsNone(parse_number("-666666666"))

    def test_safe_ratio_requires_positive_denominator(self):
        self.assertEqual(safe_ratio(2, 4), 0.5)
        self.assertIsNone(safe_ratio(2, 0))
        self.assertIsNone(safe_ratio(None, 4))

    def test_collect_acs_estimates_builds_income_poverty_and_race_measures(self):
        def write_table(directory: Path, table_id: str, rows: list[dict[str, str]]) -> Path:
            path = directory / f"{table_id}.csv.gz"
            with gzip.open(path, "wt", encoding="latin-1", newline="") as handle:
                writer = csv.DictWriter(handle, fieldnames=["GEOID", "NAME", "ORDER", "estimate"])
                writer.writeheader()
                writer.writerows(rows)
            return path

        geoid = "16000US0177256"
        name = "Tuscaloosa city, Alabama"
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            paths_by_table = {
                "B01003": write_table(root, "B01003", [
                    {"GEOID": geoid, "NAME": name, "ORDER": "1", "estimate": "1000"},
                ]),
                "B02001": write_table(root, "B02001", [
                    {"GEOID": geoid, "NAME": name, "ORDER": "1", "estimate": "1000"},
                    {"GEOID": geoid, "NAME": name, "ORDER": "3", "estimate": "200"},
                ]),
                "B17001": write_table(root, "B17001", [
                    {"GEOID": geoid, "NAME": name, "ORDER": "1", "estimate": "900"},
                    {"GEOID": geoid, "NAME": name, "ORDER": "2", "estimate": "180"},
                ]),
                "B19013": write_table(root, "B19013", [
                    {"GEOID": geoid, "NAME": name, "ORDER": "1", "estimate": "55,000"},
                ]),
            }

            row = collect_acs_estimates(paths_by_table)[geoid]

        self.assertEqual(row["acs_total_population"], 1000)
        self.assertEqual(row["acs_race_total_population"], 1000)
        self.assertEqual(row["acs_black_alone_population"], 200)
        self.assertAlmostEqual(row["acs_black_share"], 0.2)
        self.assertEqual(row["acs_poverty_universe_population"], 900)
        self.assertEqual(row["acs_poverty_population"], 180)
        self.assertAlmostEqual(row["acs_poverty_share"], 0.2)
        self.assertEqual(row["acs_median_household_income"], 55000)

    def test_collect_acs_api_estimates_uses_named_variables(self):
        payload = [
            [
                "NAME",
                "B01003_001E",
                "B02001_001E",
                "B02001_003E",
                "B17001_001E",
                "B17001_002E",
                "B19013_001E",
                "state",
                "place",
            ],
            [
                "Tuscaloosa city, Alabama",
                "1000",
                "1000",
                "200",
                "900",
                "180",
                "55000",
                "01",
                "77256",
            ],
        ]
        with tempfile.TemporaryDirectory() as temp_dir:
            path = Path(temp_dir) / "acs_api.json"
            path.write_text(json.dumps(payload), encoding="utf-8")
            row = collect_acs_api_estimates(path)["16000US0177256"]

        self.assertEqual(row["acs_place_name"], "Tuscaloosa city, Alabama")
        self.assertEqual(row["acs_total_population"], 1000)
        self.assertAlmostEqual(row["acs_black_share"], 0.2)
        self.assertAlmostEqual(row["acs_poverty_share"], 0.2)
        self.assertEqual(row["acs_median_household_income"], 55000)

    def test_match_places_prefers_exact_state_town_match(self):
        town_keys = [TownKey("TUSCALOOSA", "AL")]
        place_rows = [
            {
                "processed_state": "AL",
                "acs_processed_town": "TUSCALOOSA",
                "acs_place_name": "Tuscaloosa city, Alabama",
                "acs_geoid": "16000US0177256",
                "acs_total_population": 100000,
            }
        ]
        matched, review = match_places(town_keys, place_rows, fuzzy_threshold=0.94, fuzzy_margin=0.03)
        self.assertEqual(matched[0]["acs_match_method"], "exact")
        self.assertEqual(matched[0]["acs_geoid"], "16000US0177256")
        self.assertEqual(review, [])

    def test_match_places_accepts_high_confidence_fuzzy_within_state(self):
        town_keys = [TownKey("MOUNTAIN BRK", "AL")]
        place_rows = [
            {
                "processed_state": "AL",
                "acs_processed_town": "MOUNTAIN BROOK",
                "acs_place_name": "Mountain Brook city, Alabama",
                "acs_geoid": "16000US0151016",
                "acs_total_population": 21000,
            },
            {
                "processed_state": "AL",
                "acs_processed_town": "MONTGOMERY",
                "acs_place_name": "Montgomery city, Alabama",
                "acs_geoid": "16000US0151000",
                "acs_total_population": 200000,
            },
        ]
        matched, review = match_places(town_keys, place_rows, fuzzy_threshold=0.90, fuzzy_margin=0.03)
        self.assertEqual(matched[0]["acs_match_method"], "fuzzy")
        self.assertEqual(matched[0]["acs_geoid"], "16000US0151016")
        self.assertEqual(review, [])


if __name__ == "__main__":
    unittest.main()
