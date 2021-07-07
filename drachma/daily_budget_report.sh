VENV=/home/jbalint/sw/banshee-sympatico/drachma/venv3
. $VENV/bin/activate
$VENV/bin/jupyter nbconvert --execute --ExecutePreprocessor.timeout=60 --to html daily_budget_report.ipynb
