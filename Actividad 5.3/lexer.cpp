// =============================================================================
// File: lexer.cpp
// Author: Jose Ricardo Rosales Castaneda
// Author: Dante David Perez Perez
// Description: This file contains the implementation of a C# lexer program.
//              executing both secuentially and in parallel.    
// =============================================================================

#include <iostream>
#include <fstream>
#include <regex>
#include <pthread.h>
#include <filesystem>
#include <ctime>

using namespace std;
namespace fs = std::filesystem;

// ===================================================================
// Function prototypes
// ===================================================================
string lexer(const string &input);
void htmlFile(const string &tokenizedCode, const string &filename);
double secuentialExecution(const string &folderPath);
double parallelExecution(const string &folderPath);
void *threadFunction(void *args);

// ===================================================================
// Main function
// 
// Parameters:
//  argc: Number of arguments
//  argv: Array of arguments
// 
// Returns:
//  0 if the program executed successfully
// ===================================================================
int main(int argc, char *argv[])
{
    double secuentialTime, parallelTime;
    string folderPath = argv[1];

    // Check if the user provided the folder path
    if (argc < 2)
    {
        cout << "Usage: lexer <folder_path>" << endl;
        return 0;
    }

    // Check if the provided folder path exists
    if (!fs::exists(folderPath) || !fs::is_directory(folderPath))
    {
        cout << "Invalid folder path" << endl;
        return 0;
    }

    // Secuential execution
    secuentialTime = secuentialExecution(folderPath);

    // Parallel execution
    parallelTime = parallelExecution(folderPath);

    // Print execution times
    cout << "Secuential execution time: " << secuentialTime << " seconds" << endl;
    cout << "Parallel execution time: " << parallelTime - secuentialTime << " seconds" << endl;

    return 0;
}

// ===================================================================
// Lexer function
//
// Parameters:
//  input: Code to be tokenized
//
// Returns:
//  Tokenized code
// ===================================================================
string lexer(const string &input)
{
    string tokenizedCode;

    // Regular expressions
    const string keywords = "abstract|as|base|bool|break|byte|case|catch|char|checked|class|const|continue|decimal|default|delegate|double|do|else|enum|event|explicit|extern|false|finally|fixed|float|foreach|for|foreach|goto|if|implicit|int|in|interface|internal|is|lock|long|namespace|new|null|object|operator|out|override|params|private|protected|public|readonly|ref|return|sbyte|sealed|short|sizeof|stackalloc|static|string|struct|switch|this|throw|true|try|typeof|uint|ulong|unchecked|unsafe|ushort|using|virtual|void|volatile|while";
    const string identifiers = "[a-zA-Z_][a-zA-Z0-9_]*";
    const string operators = "\\+\\+|--|&&|\\|\\||<<|>>|<=|>=|==|!=|\\+=|-=|\\*=|/=|%=|&=|\\|=|\\^=|<<=|>>=|=>|[-+*/%&|^!=<>]=|[-+*/%&|^<>]";
    const string literals = "[0-9]+(\\.[0-9]+)?|\".*\"|'.*'";
    const string comments = "//.*|/\\*.*\\*/";
    const string system = "System|Console|Program|program";
    const string separators = "[\\(\\)\\{\\}\\[\\];,.]";
    const string lineBreak = "\n";
    const string whiteSpace = "\\s+";
    const regex allTokens(keywords + "|" + identifiers + "|" + operators + "|" + literals + "|" + comments + "|" + system + "|" + separators + "|" + lineBreak + "|" + whiteSpace);

    // Tokenize the code
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
            else if (regex_match(token, regex(operators)))
            {
                type = "operator";
            }
            else if (regex_match(token, regex(comments)))
            {
                type = "comment";
            }
            else if (regex_match(token, regex(keywords)))
            {
                type = "keyword";
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

// ===================================================================
// HTML file function
//
// Parameters:
//  tokenizedCode: Tokenized code
//  filename: Name of the file to be created
// ===================================================================
void htmlFile(const string &tokenizedCode, const string &filename)
{
    string outputFilename = fs::path(filename).stem().string() + ".html";
    string outputFilePath = "./output/" + outputFilename;

    // Create the output folder if it doesn't exist
    if (!fs::exists("./output/"))
    {
        fs::create_directory("./output/");
    }

    // Create the output file
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

    // Write the html file
    ofstream outfile(outputFilePath, ios::app);
    if (outfile.is_open())
    {
        outfile << html;
        outfile.close();
    }
    else
    {
        cout << "Error opening" + outputFilename << endl;
    }
}

// ===================================================================
// Secuential execution function
//
// Parameters:
//  folderPath: Path of the folder containing the files to be tokenized
//
// Returns:
//  Elapsed time
// ===================================================================
double secuentialExecution(const string &folderPath){
    cout << "Executing secuential code" << endl;

    // Start timer
    clock_t begin = clock();

    // Iterate over files in the folder
    for (const auto &entry : fs::directory_iterator(folderPath))
    {
        if (entry.is_regular_file() && entry.path().extension() == ".cs")
        {
            string filePath = entry.path().string();

            ifstream file(filePath);
            if (!file.is_open())
            {
                cout << "Error opening file: " << filePath << endl;
                continue;
            }

            string input((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
            string tokenizedCode = lexer(input);

            htmlFile(tokenizedCode, filePath);
        }
    }
    
    // End timer
    clock_t end = clock();

    // Calculate elapsed time
    double elapsed_secs = double(end - begin) / CLOCKS_PER_SEC;
    
    // Return elapsed time
    return elapsed_secs;
}

// ===================================================================
// Struct that contains the arguments for the thread function
// ===================================================================
struct ThreadArgs
{
    string filename;
    string folderPath;
};

// ===================================================================
// Thread function
//
// Parameters:
//  args: Thread arguments
// ===================================================================
void *threadFunction(void *args)
{
    ThreadArgs *threadArgs = (ThreadArgs *)args;
    string filename = threadArgs->filename;
    string folderPath = threadArgs->folderPath;

    // Read the file
    ifstream file(folderPath + "/" + filename);
    string input((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());

    // Tokenize the code
    string tokenizedCode = lexer(input);

    // Create the output file
    htmlFile(tokenizedCode, filename);

    pthread_exit(NULL);

    return NULL;
}

// ===================================================================
// Parallel execution function
//
// Parameters:
//  folderPath: Path of the folder containing the files to be tokenized
//
// Returns:
//  Elapsed time
// ===================================================================
double parallelExecution(const string &folderPath)
{
    cout << "Executing parallel code" << endl;

    // Start the timer
    clock_t start = clock();

    // Create the threads
    pthread_t threads[8];
    int threadIndex = 0;

    // Iterate over the files in the folder
    for (const auto &entry : fs::directory_iterator(folderPath))
    {
        string filename = entry.path().filename().string();

        // Create the thread arguments
        ThreadArgs *threadArgs = new ThreadArgs;
        threadArgs->filename = filename;
        threadArgs->folderPath = folderPath;

        // Create the thread
        pthread_create(&threads[threadIndex], NULL, threadFunction, (void *)threadArgs);

        // Increase the thread index
        threadIndex++;
    }

    // Wait for the threads to finish
    for (int i = 0; i < threadIndex; i++)
    {
        pthread_join(threads[i], NULL);
    }

    // Stop the timer
    clock_t end = clock();

    // Calculate the execution time
    double executionTime = double(end - start) / double(CLOCKS_PER_SEC);

    return executionTime;
}