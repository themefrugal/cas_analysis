# Sample CAS PDFs

These PDFs are synthetic CAS-style files for trying the Shiny app without a real investor statement.

- PAN values, folio numbers, and transactions are fictional.
- Fund names are real mutual fund scheme names.
- Transaction NAVs and closing NAVs are real historical values read from the local NAV cache when the files were generated.
- The PDFs are not password protected. Leave the app password field blank.

Regenerate them from the repository root with:

```r
Rscript tools/generate_sample_cas_pdfs.R
```
