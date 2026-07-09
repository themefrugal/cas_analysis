from pathlib import Path

import pandas as pd
import pytest

import cas_streamlit.core.parser as parser
from cas_streamlit.core.parser import CasParseError, cas_state_from_pages, get_portfolio_transactions, parse_cas_pdf


PARSER_FIXTURE_ROOT = Path("tests/fixtures/r_parser")
TRANSACTION_COLUMNS = [
    "date",
    "description",
    "amt",
    "units",
    "nav",
    "bal_units",
    "days",
    "years",
    "amc",
    "fund",
    "advisor",
    "folio",
    "pan",
]


def _read_pages(case: str) -> list[str]:
    path = PARSER_FIXTURE_ROOT / case / "pages.txt"
    if not path.exists():
        pytest.fail(f"Missing parser fixture pages: {path}. Run tools/generate_r_parser_fixtures.R.")
    return [path.read_text()]


def _normalise_transactions(df: pd.DataFrame) -> pd.DataFrame:
    out = df[TRANSACTION_COLUMNS].copy()
    out["date"] = pd.to_datetime(out["date"], errors="coerce").dt.strftime("%Y-%m-%d")
    return out.reset_index(drop=True)


@pytest.mark.parametrize("case", ["standard_two_folio", "idcw_reinvest"])
def test_python_parser_matches_r_parser_fixtures(case):
    expected_path = PARSER_FIXTURE_ROOT / case / "transactions.csv"
    if not expected_path.exists():
        pytest.fail(f"Missing parser transaction fixture: {expected_path}. Run tools/generate_r_parser_fixtures.R.")

    state = cas_state_from_pages(_read_pages(case))
    actual = _normalise_transactions(get_portfolio_transactions(state))
    expected = _normalise_transactions(pd.read_csv(expected_path))

    pd.testing.assert_frame_equal(actual, expected, check_dtype=False)


def test_python_parser_raises_like_r_for_bad_closing_balance():
    expected_error_path = PARSER_FIXTURE_ROOT / "bad_closing" / "error.txt"
    expected_message = expected_error_path.read_text().strip()

    state = cas_state_from_pages(_read_pages("bad_closing"))
    with pytest.raises(CasParseError) as exc:
        get_portfolio_transactions(state)

    assert "Could not parse closing balance" in str(exc.value)
    assert "Could not parse closing balance" in expected_message


def test_parse_cas_pdf_selects_extractor_candidate_with_transaction_rows(monkeypatch):
    bad_pages = [
        "\n".join(
            [
                "Alpha Mutual Fund",
                "Folio No: 900000010 / 10                                  PAN: AAAAA0000A",
                "ALPHA-Alpha Growth Fund (Advisor: DIRECT)",
                "Opening Unit Balance: 0.000",
                "Closing Unit Balance: 100.000    NAV: INR 110.000    Market Value on 31-Jan-2024: INR 11,000.000",
            ]
        )
    ]
    good_pages = _read_pages("standard_two_folio")

    monkeypatch.setattr(
        parser,
        "_pdf_text_candidates",
        lambda file, password="": [("bad-extractor", bad_pages), ("good-extractor", good_pages)],
    )

    state = parse_cas_pdf("unused.pdf", "")
    txns = get_portfolio_transactions(state)

    assert len(txns) == 4
    assert txns["folio"].tolist() == ["900000010 / 10", "900000011 / 11", "900000010 / 10", "900000011 / 11"]


def test_python_parser_reassembles_split_transaction_lines():
    pages = [
        "\n".join(
            [
                "Alpha Mutual Fund",
                "Folio No: 900000010 / 10                                  PAN: AAAAA0000A",
                "ALPHA-Alpha Growth Fund (Advisor: DIRECT)",
                "Opening Unit Balance: 0.000",
                "01-Jan-2024 Purchase",
                "(Continuous Offer)",
                "10,000.000    100.000       100.000    100.000",
                "Closing Unit Balance: 100.000    NAV: INR 110.000    Market Value on 31-Jan-2024: INR 11,000.000",
            ]
        )
    ]

    txns = get_portfolio_transactions(cas_state_from_pages(pages))

    purchase = txns[txns["description"] != "Cur Value"].iloc[0]
    assert purchase["description"] == "Purchase (Continuous Offer)"
    assert purchase["amt"] == 10000
    assert purchase["units"] == 100
    assert purchase["nav"] == 100
    assert purchase["bal_units"] == 100
