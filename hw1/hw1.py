# Kevin Bui
# hw1.py
# 4/12/23
# This program processes two letter pairs and counts their frequency

import sys

def countPairs(fileName):
    inFile = open(fileName)

    pairsDict = {}

    for line in inFile:

        line = line.lower() # sets letters to lowercase

        count = 0
        while (count + 1) < len(line):
            charString = line[count] + line[count + 1]
            if charString.isalpha(): # confirms if pair is all letters
                if charString in pairsDict:
                    pairsDict[charString] += 1
                else:
                    pairsDict[charString] = 1
            count += 1        
    inFile.close()
    return pairsDict


def getTopFivePairs(pairsDict):
    topFivePairsList = []
    sortedPairs = sorted(pairsDict.items(), key = lambda x: (-x[1], x[0])) 
    # sorts pairs by value first then key, descending order

    count = 0
    # stores first 5 pairs
    while count < 5 and count < len(sortedPairs): 
        topFivePairsList.append(sortedPairs[count])
        count += 1

    if len(topFivePairsList) == 0:
        return topFivePairsList

    
    if len(topFivePairsList) == 5:
        minValue = topFivePairsList[count-1][1] 
        while minValue == sortedPairs[count][1] and count < len(sortedPairs):
            topFivePairsList.append(sortedPairs[count])
            count += 1
    
    return topFivePairsList

def createFollowsDict(pairsDict, letter):
    lettersDict = {}
    for char in "abcdefghijklmnopqrstuvwxyz": # initializes array with all letters
        lettersDict[char] = 0
    for key in pairsDict:
        if key[0] == letter: # if the first letter is parameter, next one follows
            lettersDict[key[1]] += pairsDict[key] # add each count of that pair
    return lettersDict

# Main

pairs = countPairs(sys.argv[1])

keyCount = 0
pairCount = 0

for key in pairs:
    keyCount += 1
    pairCount += pairs[key]

print(keyCount)
print(pairCount)

print(getTopFivePairs(pairs))

for vowel in "aeiou":
    print(vowel)
    lettersDict = createFollowsDict(pairs, vowel)
    lettersList = [lettersDict[key] for key in "abcdefghijklmnopqrstuvwxyz"]
    print(lettersList)