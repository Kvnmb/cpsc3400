# Homework 6
import re

# REGULAR EXPRESSIONS

# Write patterns for regular expressions a-d here.
# You must use a single regular expression for each item.
# For part d, also include a substitution string.

a = re.compile(r"^[A-Za-z]*qu[A-Za-z]*zz[A-Za-z]*$|^[A-Za-z]*zz[A-Za-z]*qu[A-Za-z]*$")      

b = re.compile(r"^(\(\d{3}\)\s?|\d{3}-)\d{3}-\d{4}$")       

c = re.compile(r"^\[([1-9]\d*;\s*)*[1-9]\d*\]$")   

d = re.compile(r"^([^?|^:]+?)\s*\?\s*([^?|^:]+?)\s*:\s*([^?|^:]+?)$")
subStr = r"\2 if \1 else \3"   # Place what you want to substitute (used in sub)

# TESTS

print("----Part a tests that match:")
print(a.search("aquapizza"))

print("----Part a tests that do not match:")
print(a.search("aquataco"))

print("----Part b tests that match:")
print(b.search("(555) 123-4567"))

print("----Part b tests that do not match:")
print(b.search("(555)-123-4567"))

print("----Part c tests that match:")
print(c.search("[1; 4; 6; 12; 3; 70]"))

print("----Part c tests that do not match:")
print(c.search("[1; 4; hi; 12; 3; 70]"))

print("----Part d tests:")
print(d.sub(subStr, "a < b ? x : 3 + y"))
