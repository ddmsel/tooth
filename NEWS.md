# tooth 0.5.0

## Naming

* The arch visualization is now consistently called an **odontogram**
  throughout the package. `build_odontograph()` has been renamed to
  `build_odontogram()`.
* `build_odontograph()` is retained as a **deprecated** alias that forwards
  to `build_odontogram()` with a warning, and will be removed in a future
  release.

## Caries activity handling

* `calc_dmft()` and `calc_dmfs()` gain arguments to control whether caries
  **activity** is required for a lesion to be scored as decayed:
  * `consider_activity` (default `TRUE`) — the overall setting.
  * `consider_activity_coronal` / `consider_activity_root` (default `NULL`,
    i.e. inherit the overall setting) — per-component overrides.
* Set `consider_activity = FALSE` to count every lesion in `decayed_codes`
  regardless of activity, matching the conventional epidemiological D(3)MFT
  definition. When activity is not considered, the activity column need not
  be present.
* **Behavior change:** root caries now honours the activity setting as well.
  Previously root lesions were counted regardless of activity, which was
  inconsistent with the coronal calculation. To restore the old
  root behaviour, pass `consider_activity_root = FALSE`.

## Tooth numbering

* **Behavior change:** for **primary** dentition, `tooth_convert()` now
  returns the standard ADA Universal **letters A–T** (A = upper-right second
  primary molar … T = lower-right second primary molar) instead of the
  previous numeric 51–70 encoding. Permanent Universal numbering (1–32) is
  unchanged.

## Documentation

* Documented the default `decayed_codes = c(3, 4, 5, 6)` as the **D3
  threshold** (ICDAS 3–6, i.e. cavitated lesions; enamel-only ICDAS 1–2
  lesions treated as sound), with guidance on using a D1 threshold instead.
