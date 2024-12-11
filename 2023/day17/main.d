import core.bitop, std.bitmanip;
import core.checkedint;
import core.time;
import std.algorithm, std.functional, std.meta;
import std.array, std.container;
import std.bigint;
import std.conv;
import std.format;
import std.math, std.numeric;
import std.range, std.range.interfaces;
import std.stdio, std.string;
import std.ascii, std.typecons;
import std.datetime.date, std.datetime.systime;

void main(string[] args) {
    string[] input;
    string line;
    while ((line = readln.strip) !is null) { input ~= line; }

    auto res = solveEasy(input);
    // auto res = solveHard(input);

    res.writeln;
}

alias Dir = Tuple!(int, "x", int, "y");
immutable Dir Up = Dir(-1, 0);
immutable Dir Right = Dir(0, 1);
immutable Dir Down = Dir(1, 0);
immutable Dir Left = Dir(0, -1);

Dir[] getTurnDirs(Dir d) {
    if (d == Up || d == Down) { return [Left, Right]; }
    return [Up, Down];
}

alias Pos = Tuple!(int, "x", int, "y");
alias Node = Tuple!(Pos, "pos", Dir, "dir");
alias QNode = Tuple!(int, "dist", Node, "node");

int solveEasy(string[] input) {
    bool inbounds(Pos p) => 0 <= p.x && p.x < input.length && 0 <= p.y && p.y < input[0].length;

    Pos addDir(Pos p, Dir d) { return Pos(p.x + d.x, p.y + d.y); }

    auto directions = [Up, Right, Down, Left];

    auto vis = make!(RedBlackTree!Node);
    int[Dir][Pos] distMap;
    auto startPos = Pos(0, 0);
    auto startDist = input[0][0] - '0';
    auto q = redBlackTree(
        QNode(startDist, Node(startPos, Down)), 
        QNode(startDist, Node(startPos, Right)));

    while (!q.empty) {
        auto e = q.front;
        q.removeFront;

        //debug { q.writeln; }

        if (e.node in vis) { continue; }
        vis.insert(e.node);

        auto newDirs = getTurnDirs(e.node.dir);
        foreach (newDir; newDirs) {
            auto dist = 0;
            foreach (_; 1 .. 4) {
                auto nxtPos = addDir(e.node.pos, newDir);
                if (!inbounds(nxtPos)) { continue; }

                auto nxtNode = Node(nxtPos, newDir);
                dist += input[nxtPos.x][nxtPos.y] - '0';

                auto nxtQNode = QNode(e.dist + dist, nxtNode);
                if (nxtPos !in distMap || newDir !in distMap[nxtPos] || nxtQNode.dist < distMap[nxtPos][newDir]) {
                    distMap[nxtPos][newDir] = nxtQNode.dist;
                    q.insert(nxtQNode);
                }
            }
        }
    }

    auto end = Pos(input.length - 1, input[0].length - 1);
    return distMap[end].values.minElement;
}