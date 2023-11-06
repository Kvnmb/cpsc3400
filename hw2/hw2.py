# Kevin Bui
# hw2.py
# 4/19/23
# This program creates a binary search tree of integers using lists and performs bst operations

import sys

class DuplicateEntry(Exception):
    pass


def insert(tree, value):
    
    if len(tree) == 0: # value is new, add to tree
        tree.append(value) # node
        tree.append([]) # left subtree
        tree.append([]) # right subtree
    elif value < tree[0]: # value is less than node, go left subtree
        insert(tree[1],value)
    elif value > tree[0]:# value is greater than node, go right subtree
        insert(tree[2],value)
    else:
        try:
            raise DuplicateEntry # exception for duplicate numbers in tree
        except DuplicateEntry:
            print("Duplicate value detected: ", value)
            pass
            
            
def search(tree, value): # searches tree for specific value
    if len(tree) == 0: # empty subtree, no match
        return False
    elif value == tree[0]: # value found
        return True
    elif value < tree[0]: # traverse left subtree
        return search(tree[1],value)
    elif value > tree[0]: # traverse right subtree
        return search(tree[2],value)


def inorder(tree): # inorder traversal
    if len(tree) == 0:
        return
    else: # prints tree nodes left-middle-right
        yield from inorder(tree[1])
        yield tree[0]
        yield from inorder(tree[2])

def heights(tree):
    def heightsHelper(subtree, height): # helper function that holds the dictionary to store values
        if len(subtree) == 0: # end of tree or empty list
            return
        heightsDict[subtree[0]] = height # set the height for the value
        # the further down the tree, increase the height by 1
        heightsHelper(subtree[1], height + 1) 
        heightsHelper(subtree[2], height + 1)

    heightsDict = {}
    heightsHelper(tree, 0)
    return heightsDict



# Main

inFile = open(sys.argv[1])

bst = []

for line in inFile:
    line = line.rstrip()
    try:
        value = int(line) # converts line to integer if valid
    except ValueError: # exception if line is not an integer, continues with next line
        print("Not an int:", line)
        continue
    insert(bst, value) # if value is int, insert into bst

print("Step 3: ")
print(bst)

print("Step 4:")

for i in range (1,10): # loop to search for values 1-9
    if search(bst, i):
        print(i, "YES")
    else:
        print(i, "NO")

print("Step 5:")

for value in inorder(bst): # for loop for inorder traversal
    print(value)

print("Step 6:")
inorderList = [value for value in inorder(bst)] # list comprehension inorder traversal
print(inorderList)

print("Step 7:")
heightsDict = heights(bst)
print(heightsDict)

print("Step 8:")
if len(bst) == 0:
    print(bst)
else:
    print(max(heightsDict.values()))