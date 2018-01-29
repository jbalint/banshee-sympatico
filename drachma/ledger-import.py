# Made a special build of ledger with python support. it created libledger.so.3 with Python support
# export PYTHONPATH=/home/jbalint/aur/ledger-git/src/ledger
import ledger

import re

print """
@prefix ledger: <https://github.com/jbalint/ledger-ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
"""

file = "main.ledger"
#file = "m1.ledger"

# save all expense/income categories and assert them
categories = {}

acct_reconciliations = {}
reconciliations = []

def is_expense(acctname):
    return acctname.startswith("Expenses:")

def rdf_name(acctname):
    v1 = re.sub(r".*:", "", acctname)
    v2 = v1.replace(" ", "")
    return "ledger:" + v2

def rdf_cat(cat):
    label = re.sub(r".*:", "", cat)
    cat = re.sub(r":(\w)", lambda m: m.group(1).upper(), cat)
    cat = re.sub(r" (\w)", lambda m: m.group(1).upper(), cat)
    categories[cat] = label
    return cat

def escape(astr):
    return astr.replace("\"", "\\\"")

def print_post(post):
    print "  ledger:post [ a ledger:Post ; "
    print "    ledger:category ledger:%s ;" % (rdf_cat(post.account.fullname()))
    print "    ledger:amount \"%s\"^^xsd:decimal ; " % (post.amount.number())
    if post.note:
        note = post.note.strip()
        # this one still has leading ; (comment chars) for some reason
        note = re.sub(r"^;*", "", note)
        note = note.strip()
        print "    ledger:note \"%s\" ;" % (escape(note))
    print "  ] ;"

def print_reconcile(acctname, post):
    shortname = re.sub(".*:", "", rdf_name(acctname))
    if not acctname in acct_reconciliations:
        acct_reconciliations[acctname] = 0
    num = acct_reconciliations[acctname]
    print "  ledger:reconciledAt ledger:Reconciliation-%s-%s ; " % (shortname, num)
    if post.assigned_amount:
        amt = post.assigned_amount.number()
        r = "ledger:Reconciliation-%s-%s a ledger:AccountReconciliation ;\n" % (num, shortname)
        r = r + "  ledger:account %s ; \n" % (rdf_name(acctname))
        r = r + "  ledger:balance \"%s\"^^xsd:decimal ; \n" % (amt)
        r = r + "  ledger:time \"%sT00:00:00Z\"^^xsd:dateTime ; \n" % (post.date)
        r = r + ".\n"
        acct_reconciliations[acctname] = num + 1
        reconciliations.append(r)

for xact in ledger.read_journal(file).xacts():
    first = True
    is_transfer = False
    is_income = False
    income_cat = None
    for post in xact.posts():
        acctname = post.account.fullname()
        if first:
            first = False
            if is_expense(acctname):
                print "[] a ledger:Payment ; "
                print "  ledger:payee \"%s\" ;" % (escape(xact.payee))
                print_post(post)
            elif acctname.startswith("Income:"):
                is_income = True
                print "[] a ledger:Income ; "
                print "  ledger:payor \"%s\" ;" % (xact.payee)
                income_cat = rdf_cat(post.account.fullname())
            else:
                is_transfer = True
                print "[] a ledger:Transfer ; "
                print "  ledger:destination %s ; " % (rdf_name(acctname))
                print "  ledger:post [ a ledger:Post ; "
                print "    ledger:amount \"%s\"^^xsd:decimal ; ]; " % (post.amount.number())
            note = xact.note
            if note:
                note = note.strip()
                print "  ledger:note \"%s\" ;" % (escape(note))
            print "  ledger:time \"%sT00:00:00Z\"^^xsd:dateTime ;" % (post.date)
        elif is_income:
            print "  ledger:post [ a ledger:Post ; "
            print "    ledger:category ledger:%s ;" % (income_cat)
            print "    ledger:amount \"%s\"^^xsd:decimal ; " % (post.amount.number())
            print "  ] ; "
            print "  ledger:destination %s ; " % (rdf_name(acctname))
            print_reconcile(acctname, post)
        elif is_transfer:
            print "  ledger:source %s ; " % (rdf_name(acctname))
            print_reconcile(acctname, post)
        elif not is_expense(acctname):
            print "  ledger:source %s ; " % (rdf_name(acctname))
            print_reconcile(acctname, post)
        else:
            print_post(post)
    
    print "."

for c in sorted(categories.iterkeys()):
    print "ledger:%s a ledger:Category ; rdfs:label \"%s\" . " % (c, categories[c])

for p in reconciliations:
    print p

# Why is this failing?
# RuntimeError: Assertion failed in "/home/jbalint/aur/ledger-git/src/ledger/src/session.cc", line 182:std::size_t ledger::session_t::read_data(const string&): xact_count == journal->xacts.size()
#for post in ledger.read_journal(file).query("food"):
#    print "Transferring %s to/from %s" % (post.amount, post.account)
