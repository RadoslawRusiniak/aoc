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

alias CubeInfo = Tuple!(int, "count", string, "color");
alias Draw = CubeInfo[];
alias Game = Tuple!(int, "id", Draw[], "draws");

void main(string[] args) {
    string[] input;
    string line;
    while ((line = readln()) !is null) { input ~= line; }

    auto games = parseInput(input);

    debug { games[0 .. 3].each!(g => g.draws[0].writeln); }

    // auto res = solveEasy(games);
    auto res = solveHard(games);

    res.writeln;
}

Game[] parseInput(string[] input) {
    CubeInfo parseCubeInfo(string input) {
        int cnt; string clr;
        input.strip.formattedRead("%d %s", cnt, clr);
        return CubeInfo(cnt, clr);
    }

    Draw parseDraw(string input) { 
        return input.split(",").map!parseCubeInfo.array;
        // Draw d;
        // d = input.strip.unformatValue!Draw("%(%d %s, %)");
        // return d;
    }

    Game parseLine(string input) {
        auto gameNumberAndDraws = input.split(":");

        int id;
        gameNumberAndDraws[0].formattedRead("Game %d", id);

        auto draws = gameNumberAndDraws[1].split(";").map!parseDraw.array;

        return Game(id, draws);
    }

    return input.map!parseLine.array;
}

int solveEasy(Game[] games) => games.filter!possibleGame.map!(g => g.id).sum;

bool possibleGame(Game game) {
    auto bag = ["red": 12, "green": 13, "blue": 14];

    bool possibleCubeInfo(CubeInfo cubeInfo) => 
        bag.keys.canFind(cubeInfo.color) && cubeInfo.count <= bag[cubeInfo.color];

    bool possibleDraw(Draw draw) => draw.all!possibleCubeInfo;

    return game.draws.all!possibleDraw;
}

int powerOfCubesInTheGameFunctional(Game game) {
    int maxForColor(string color) {
        bool isOfRelevantColor(CubeInfo cubeInfo) => cubeInfo.color == color;

        int maxForDraw(Draw draw) => 
            draw.filter!isOfRelevantColor.map!(ci => ci.count).maxElement(0);

        return game.draws.map!maxForDraw.maxElement;
    }

    return ["red", "green", "blue"].map!maxForColor.fold!((a, b) => a * b);
}

int powerOfCubesInTheGame(Game game) {
    auto bag = ["red": 0, "green": 0, "blue": 0];
    
    void updateMaxForCubeInfo(CubeInfo cubeInfo) {
        bag[cubeInfo.color] = max(cubeInfo.count, bag[cubeInfo.color]);
    }

    void updateMaxForDraw(Draw draw) { draw.each!updateMaxForCubeInfo; }

    game.draws.each!updateMaxForDraw;

    return bag["red"] * bag["green"] * bag["blue"];
}

int solveHard(Game[] games) => games.map!powerOfCubesInTheGame.sum;