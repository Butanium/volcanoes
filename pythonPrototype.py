import sys
import math
import time
# Win by building an unbroken chain of volcanoes from one side of the board to the other.

number_of_tiles = int(input())  # Number of tiles on the board
nameDic = {}
indexDic = {}
neigboursDic = {}
scoreDic = {}
for i in range(number_of_tiles):
    inputs = input().split()
    tile_name = inputs[0]  # Name of the tile at this index (e.g., N01 or S25)
    neighbor_1 = int(inputs[1])  # Index of a neighboring tile
    neighbor_2 = int(inputs[2])  # Index of a neighboring tile
    neighbor_3 = int(inputs[3])  # Index of a neighboring tile
    nameDic[i] = tile_name
    indexDic[tile_name] = i
    neigboursDic[i] = [neighbor_1, neighbor_2, neighbor_3]
    scoreDic[i] = 0
def debug(m):
    print(m, file=sys.stderr, flush=True)

# game loop:
n = 0
s = 0
def getOpp(c):
    return ("N" if c[0]=="S" else "S")+ c[1:]

while True:
    t = time.time()
    for i in scoreDic:
        scoreDic[i] = 0


    position = [int(i) for i in input().split()]  # Space-separated list of the volcano levels for every tile on the board (in index order); value will be positive for your volcanoes, negative for your opponent's volcanoes, or 0 if empty
    moves = input().split()  # Space-separate list of all valid moves in this position
    # debug(moves)
    goal = [getOpp(nameDic[i]) for i in range(len(position)) if position[i]>0]
    # debug("goal : {}".format(goal))

    allyDetect = lambda x : sum(position[i]>0 for i in neigboursDic[x])
    allySum = lambda x : sum(max(0, position[i]) for i in neigboursDic[x])
    for i in map(lambda x : indexDic[x], goal):
        if getOpp(nameDic[i]) not in goal or (allyDetect(i) == allyDetect(indexDic[getOpp(nameDic[i])])):
            scoreDic[i] = 100
        elif not allyDetect(i) and  allyDetect(indexDic[getOpp(nameDic[i])]):
            scoreDic[i] = 500
    score = lambda m : sum(max(0,position[i]) for i in neigboursDic[indexDic[m]])+ (n if m[0] == 'N' else s)
    score = lambda m : scoreDic[indexDic[m]] - position[indexDic[m]]
    k = 0
    while time.time() - t < .09:
        newScores = scoreDic.copy()
        for i in scoreDic:
            if time.time() - t < .09:
                break
            newScores[i] = max(scoreDic[i],max(scoreDic[j]/2 for j in neigboursDic[i]))
        else:
            k+= 1
        scoreDic = newScores

    sortedMoves = list(filter(lambda x : (position[indexDic[x]]>0)+allyDetect(indexDic[x]) + sum(allyDetect(i) for i in neigboursDic[indexDic[x]]),moves))
    sortedMoves.sort(key = score, reverse=True)
    # debug("scDic : {}".format(list(filter(lambda t : scoreDic[indexDic[t[0]]]>0,list(map(lambda x : (nameDic[x],scoreDic[x]), scoreDic.keys()))))))
    # debug(list(zip(map(lambda x : x, sortedMoves), map(score, sortedMoves))))
    # Write    an action using print
    # To # debug:
    debug("k : {}".format(k))
    # Either RANDOM or a tile name (e.g., N12 or S34)
    if sortedMoves:
        if sortedMoves[0][0] == "N":
            n += 1
        else:
            s += 1
        # debug("s : {}, n : {}".format(s,n))
        print(sortedMoves[0])
    else:
        print("S36" if "S36" in moves else "random")
