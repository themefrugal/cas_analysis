import importlib.util

import pytest

from cas_streamlit.core.parser import get_portfolio_transactions, parse_cas_pdf, sample_pdf_paths


@pytest.mark.skipif(
    importlib.util.find_spec("pypdf") is None and importlib.util.find_spec("PyPDF2") is None,
    reason="pypdf/PyPDF2 is not installed",
)
def test_bundled_sample_pdfs_parse_through_python_parser():
    samples = sample_pdf_paths()
    assert len(samples) >= 3

    for sample in samples:
        state = parse_cas_pdf(sample, password="")
        txns = get_portfolio_transactions(state)

        assert not txns.empty
        assert "Cur Value" in txns["description"].tolist()
        assert txns["pan"].str.match(r"^[A-E]{5}[0-4]{4}[A-E]$").all()
        assert txns["folio"].str.match(r"^90000000[1-5]").all()
