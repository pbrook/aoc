#! /usr/bin/env python3

grid={}
def getchar():
    global regex
    c=regex[0]
    regex=regex[1:]
    return c

def parse(startx, starty, start):
    dist=start
    x=startx
    y=starty
    while True:
        c = getchar()
        if c == '(':
            parse(x, y, dist)
        elif c == '|':
            dist=start
            x=startx
            y=starty
        elif c == ')' or c == '$':
            return
        else:
            dist += 1
            if c == 'N':
                y-=1
            elif c == 'E':
                x += 1
            elif c == 'S':
                y += 1
            elif c == 'W':
                x -= 1
            if (x,y) in grid:
                prev=grid[(x,y)]
                if prev < dist:
                    dist=prev
            grid[(x,y)]=dist

with open("input.txt") as f:
    regex = f.readline()

#regex='^ENWWW(NEEE|SSE(EE|N))$'
#regex='^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$'
#regex='^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$'
#regex='^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$'
getchar()
parse(0,0,0)
print(max(grid.items(), key=lambda x: x[1]))
