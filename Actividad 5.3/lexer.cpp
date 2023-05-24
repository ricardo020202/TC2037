#include <iostream>
#include <fstream>
#include <regex>
#include <pthread.h>

using namespace std;

string lexer(const string& input);
void htmlFile(string tokenizedCode);

int main()
{
    ifstream file("input.cs");
    if (!file.is_open())
    {
        cout << "Error opening file" << endl;
        return 0;
    }

    string input((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
    string tokenizedCode = lexer(input);

    htmlFile(tokenizedCode);

    return 0;
}

string lexer(const string& input)
{
    string tokenizedCode;

    const string keywords = "abstract|as|base|bool|break|byte|case|catch|char|checked|class|const|continue|decimal|default|delegate|do|double|else|enum|event|explicit|extern|false|finally|fixed|float|foreach|for|foreach|goto|if|implicit|int|in|interface|internal|is|lock|long|namespace|new|null|object|operator|out|override|params|private|protected|public|readonly|ref|return|sbyte|sealed|short|sizeof|stackalloc|static|string|struct|switch|this|throw|true|try|typeof|uint|ulong|unchecked|unsafe|ushort|using|virtual|void|volatile|while";
    const string identifiers = "[a-zA-Z_][a-zA-Z0-9_]*";
    const string operators = "\\+\\+|--|&&|\\|\\||<<|>>|<=|>=|==|!=|\\+=|-=|\\*=|/=|%=|&=|\\|=|\\^=|<<=|>>=|=>|[-+*/%&|^!=<>]=|[-+*/%&|^<>]";
    const string literals = "[0-9]+(\\.[0-9]+)?|\".*\"|'.*'";
    const string comments = "//.*|/\\*.*\\*/";
    const string system = "System|Console|Program|program";
    const string separators = "[\\(\\)\\{\\}\\[\\];,.]";
    const string lineBreak = "\n";
    const string whiteSpace = "[ \t\r\f\v]+";

    const regex allTokens(keywords + "|" + identifiers + "|" + operators + "|" + literals + "|" + comments + "|" + system + "|" + separators + "|" + lineBreak + "|" + whiteSpace);

    auto current = sregex_iterator(input.begin(), input.end(), allTokens);
    const auto end = sregex_iterator();

    while (current != end)
    {
        const string token = (*current).str();

        if (token == "\n")
        {
            tokenizedCode += "<br>";
        }
        else
        {
            string type;

            if (regex_match(token, regex(lineBreak)))
            {
                tokenizedCode += "</pre><pre>";
            }
            else if (regex_match(token, regex(whiteSpace)))
            {
                tokenizedCode += token;
            }
            else if (regex_match(token, regex(comments)))
            {
                type = "comment";
            }
            else if (regex_match(token, regex(keywords)))
            {
                type = "keyword";
            }
            else if (regex_match(token, regex(operators)))
            {
                type = "operator";
            }
            else if (regex_match(token, regex(literals)))
            {
                type = "literal";
            }
            else if (regex_match(token, regex(system)))
            {
                type = "System";
            }
            else if (regex_match(token, regex(separators)))
            {
                type = "separator";
            }
            else if (regex_match(token, regex(identifiers)))
            {
                type = "identifier";
            }
            else
            {
                type = "error";
            }

            tokenizedCode += "<span class=\"" + type + "\">" + token + "</span>";
        }

        ++current;
    }
    return tokenizedCode;
}

void htmlFile(string tokenizedCode){
    string html = R"(
        <!DOCTYPE html>
        <html>
        <head>
        <style>
            body {
                background-color: #2b2b2b;
                color: #f8f8f2;
                font-family: Consolas, monospace;
                font-size: 14px;
                margin: 30px;
            }
            pre {
                margin: -15px;
            }
            span {
                display: inline-block;
            }
            span.keyword {
                color: #ff0000;
            }
            span.identifier {
                color: #cccccc;
            }
            span.operator {
                color: #d10000;
            }
            span.literal {
                color: #bf00ff;
            }
            span.comment {
                color: #75715e;
            }
            span.System {
                color: #8453ff;
            }
            span.separator {
                color: #ffff00;
            }
        </style>
        </head>
        <body>
        <pre>
    )";

    html += tokenizedCode;

    html += R"(
        </pre>
        </body>
        </html>
    )";

    ofstream outfile("output.html", ios::app);
    if (outfile.is_open())
    {
        outfile << html;
        outfile.close();
    }
    else
    {
        cout << "Error opening output.html" << endl;
    }
}
