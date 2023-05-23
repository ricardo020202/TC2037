// C# lexer using c++ regex
// categories of tokens: keywords, identifiers, operators, literals, and comments

#include <iostream>
#include <string>
#include <regex>
#include <fstream>
using namespace std;

// function prototypes
void lexer(string input);
void print(string token, string type);
void html(string token, string type);

int main()
{
    // read file input
    ifstream file("input.cs");

    if (!file.is_open())
    {
        cout << "Error opening file" << endl;
        return 0;
    }

    // convert file input to string
    string input((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());

    // call lexer function
    lexer(input);

    return 0;
}

// lexer function
void lexer(string input)
{
    // regex patterns
    regex keywords("abstract|as|base|bool|break|byte|case|catch|char|checked|class|const|continue|decimal|default|delegate|do|double|else|enum|event|explicit|extern|false|finally|fixed|float|for|foreach|goto|if|implicit|in|int|interface|internal|is|lock|long|namespace|new|null|object|operator|out|override|params|private|protected|public|readonly|ref|return|sbyte|sealed|short|sizeof|stackalloc|static|string|struct|switch|this|throw|true|try|typeof|uint|ulong|unchecked|unsafe|ushort|using|virtual|void|volatile|while");
    regex identifiers("[a-zA-Z_][a-zA-Z0-9_]*");
    regex operators("[+\\-*/%=]|==|!=|<=|>=|<|>|&&|\\|\\|");
    regex literals("[0-9]+(\\.[0-9]+)?|\".*\"|'.*'");
    regex comments("//.*|/\\*.*\\*/");
    regex System("System|Console|Program");
    regex separators("[\\(\\)\\{\\}\\[\\];,]");

    // iterator
    sregex_iterator current(input.begin(), input.end(), keywords);
    sregex_iterator end;
    while (current != end)
    {
        print(current->str(), "keyword");
        current++;
    }

    current = sregex_iterator(input.begin(), input.end(), identifiers);
    while (current != end)
    {
        print(current->str(), "identifier");
        current++;
    }

    current = sregex_iterator(input.begin(), input.end(), operators);
    while (current != end)
    {
        print(current->str(), "operator");
        current++;
    }

    current = sregex_iterator(input.begin(), input.end(), literals);
    while (current != end)
    {
        print(current->str(), "literal");
        current++;
    }

    current = sregex_iterator(input.begin(), input.end(), comments);
    while (current != end)
    {
        print(current->str(), "comment");
        current++;
    }

    current = sregex_iterator(input.begin(), input.end(), System);
    while (current != end)
    {
        print(current->str(), "System");
        current++;
    }

    current = sregex_iterator(input.begin(), input.end(), separators);
    while (current != end)
    {
        print(current->str(), "separator");
        current++;
    }
}

// print function
void print(string token, string type)
{
    cout << token << " - " << type << endl;
}

// html function
void html(string token, string type)
{
    cout << "<span class=\"" << type << "\">" << token << "</span>" << endl;
}