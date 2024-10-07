using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using Microsoft.FSharp.Collections;
using OxyPlot;
using OxyPlot.Series;
using System.Text.RegularExpressions;

namespace InterpreterGUI
{

    public partial class MainWindow : Window
    {
        private Dictionary<string, string> symbolTable = new Dictionary<string, string>();

        public MainWindow()
        {
            InitializeComponent();
        }

        private void EvaluateButton_Click(object sender, RoutedEventArgs e)
        {
            string expression = ExpressionTextBox.Text;
            try
            {
                var lexerResult = Interpreter2.lexer(expression);
                var parserResult = Interpreter2.parser(lexerResult);

                // Before passing the symbolTable to parseNeval, convert numeric string values to double
                var fsharpList = ListModule.OfSeq(symbolTable
                    .Where(kv => !string.IsNullOrEmpty(kv.Value) && double.TryParse(kv.Value, out _))
                    .Select(kv => Tuple.Create(kv.Key, double.Parse(kv.Value))));

                var evaluationResult = Interpreter2.parseNeval(lexerResult, fsharpList);
                float result = (float)evaluationResult.Item2.Item2;
                string variableName = evaluationResult.Item2.Item1;

                // If the variable name is not empty, update the symbol table with the result
                if (!string.IsNullOrEmpty(variableName))
                    symbolTable[variableName] = result.ToString();

                ResultsTextBox.Text = $"{expression} = {result}";
                UpdateSymbolTableLabel();
                ErrorsTextBox.Text = ""; // Clear any previous errors
            }
            catch (Exception ex)
            {
                ResultsTextBox.Text = ""; // Clear any previous results
                ErrorsTextBox.Text = $"Error: {ex.Message}";
            }
        }

        private void AssignFunctionButton_Click(object sender, RoutedEventArgs e)
        {
            string function = FunctionTextBox.Text;
            try
            {
                // Split the function expression to get the variable name and the function
                string[] parts = function.Split('=');
                if (parts.Length == 2 && parts[0].Trim() == "y")
                {
                    // Extract the function expression
                    string functionExpression = parts[1].Trim();

                    // Store the function expression as a string in the symbol table with the variable name "y"
                    symbolTable["y"] = functionExpression;

                    UpdateSymbolTableLabel(); // Update the display of the symbol table
                                              // Optionally, you can provide feedback to the user to indicate that the function has been assigned.
                    MessageBox.Show("Function assigned successfully!", "Assignment Status", MessageBoxButton.OK, MessageBoxImage.Information);
                }
                else
                {
                    // If the input is not in the correct format or if the variable name is not "y", show an error message to the user
                    MessageBox.Show("Invalid input format. Please enter the assignment in the format 'y = expression'.", "Input Error", MessageBoxButton.OK, MessageBoxImage.Error);
                }
            }
            catch (Exception ex)
            {
                // Handle any exceptions that may occur during the assignment process
                MessageBox.Show($"Error assigning function: {ex.Message}", "Assignment Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }


        private void Help_Click(object sender, RoutedEventArgs e)
        {
            // Explanation of valid tokens and syntax
            string helpMessage = "Valid Tokens:\n" +
                                 "+ - * / ^ % ( )\n" +
                                 "Numeric values\n" +
                                 "Variable names (e.g., x, y)\n\n" +
                                 "Syntax:\n" +
                                 "<varID>    ::= [a-z,A-Z]+ \n" +
                                 "<VA> ::= <varID>  =  <E> \n" +
                                 "<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 \n " +
                                 "<integer> ::= <digit> { <digit> } \n" +
                                 "<float> ::= <digit> { <digit> } . { <digit> } \n " +
                                 "<value> ::= <float> | <integer>| varVal \n" +
                                 "<E> ::= <T> <Eopt> \n" +
                                 "<Eopt> ::=  +  <T> <Eopt> |  -  <T> <Eopt>| <empty> \n" +
                                 "<T> ::= <NR> <Topt> \n " +
                                 "<Topt> ::=  *  <NR> <Topt> |  /  <NR> <Topt> | ^  <NR> <Topt> |  %  <NR> <Topt> |  <empty>" +
                                 "<NR> ::= ( + | - )<NR> | <value>  | ( <E> ) \n ";

            // Show the help message in a message box
            MessageBox.Show(helpMessage, "Syntax and Tokens", MessageBoxButton.OK, MessageBoxImage.Information);
        }

        private void UpdateSymbolTableLabel()
        {
            SymbolTableTextBox.Text = "";
            foreach (var entry in symbolTable)
            {
                SymbolTableTextBox.Text += $"{entry.Key} = {entry.Value}\n";
            }
        }

        private void DrawFunctionButton_Click(object sender, RoutedEventArgs e)
        {
            string function = FunctionTextBox.Text.Trim();

            // Check if the function string is empty
            if (string.IsNullOrEmpty(function))
            {
                MessageBox.Show("Please enter a function before drawing.", "No Function Entered", MessageBoxButton.OK, MessageBoxImage.Warning);
                return;
            }

            try
            {
                // Clear previous series from the plot
                FunctionPlot.Model.Series.Clear();
                double startX, endX, stepX;

                // Parse the x range input from the text boxes
                if (!double.TryParse(StartXTextBox.Text, out startX) ||
                    !double.TryParse(EndXTextBox.Text, out endX) ||
                    !double.TryParse(StepXTextBox.Text, out stepX))
                {
                    MessageBox.Show("Invalid input for x range.", "Input Error", MessageBoxButton.OK, MessageBoxImage.Error);
                    return;
                }

                // Create a new LineSeries for the function plot
                var series = new LineSeries();


                // Check if the function is linear
                if (IsLinear(function))
                {
                    // Generate points for the linear function and add them to the series
                    for (double x = startX; x <= endX; x += stepX)
                    {
                        double y = EvaluateLinear(function, x);
                        series.Points.Add(new DataPoint(x, y));
                    }
                }
                else
                {
                    // Generate points for the polynomial function and add them to the series
                    for (double x = startX; x <= endX; x += stepX)
                    {
                        double y = EvaluatePolynomial(function, x);
                        series.Points.Add(new DataPoint(x, y));
                    }
                }


                // Add the series to the plot
                FunctionPlot.Model.Series.Add(series);

                // Refresh the plot
                FunctionPlot.InvalidatePlot(true);
            }
            catch (Exception ex)
            {
                // Handle any errors that may occur during function evaluation or plotting
                MessageBox.Show($"Error drawing function: {ex.Message}", "Drawing Error", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }


        private void FunctionPlot_Loaded(object sender, RoutedEventArgs e)
        {
            // Ensure that the PlotModel is initialized
            FunctionPlot.Model = new PlotModel();
        }

        private bool IsLinear(string function)
        {
            // Check if the function contains terms like "x" or "mx + c"
            return Regex.IsMatch(function, @"\bx\b|^\s*-?\d*x\s*(\+\s*\d+)?\s*$");
        }

        private double EvaluatePolynomial(string function, double x)
        {
            // Parse the function string to extract coefficients and exponents
            MatchCollection matches = Regex.Matches(function, @"(?<coef>-?\d*)\*?x\^?(?<exp>\d+)?");

            double result = 0;
            foreach (Match match in matches)
            {
                double coef = string.IsNullOrEmpty(match.Groups["coef"].Value) ? 1 : double.Parse(match.Groups["coef"].Value);
                int exp = string.IsNullOrEmpty(match.Groups["exp"].Value) ? 1 : int.Parse(match.Groups["exp"].Value);

                result += coef * Math.Pow(x, exp);
            }

            return result;
       
            //   - (?<coef>-?\d*): Named capture group 'coef' matches an optional minus sign ('-') followed by zero or more digits ('\d*'). 
            //                      It captures the coefficient of the polynomial term.
            //   - \*?: Matches the multiplication operator '*' literally, allowing for an optional multiplication operator between the coefficient and the variable 'x'.
            //   - x: Matches the variable 'x' literally.
            //   - \^?: Matches the exponentiation operator '^' literally, allowing for an optional exponentiation operator after the variable 'x'.
            //   - (?<exp>\d+)?:
            //     - (?<exp>: Named capture group 'exp' matches one or more digits ('\d+'). It captures the exponent of the polynomial term.
            //     - ?: Makes the entire group optional to handle cases where there is no explicit exponent for a term.
            

        }

        private double EvaluateLinear(string function, double x)
        {
            // Parse the function string to extract slope and intercept
            Match match = Regex.Match(function, @"(?<m>-?\d*)x\s*(\+\s*(?<c>-?\d+))?");

            double m = string.IsNullOrEmpty(match.Groups["m"].Value) ? 0 : double.Parse(match.Groups["m"].Value);
            double c = string.IsNullOrEmpty(match.Groups["c"].Value) ? 0 : double.Parse(match.Groups["c"].Value);

            return m * x + c;
        
            //   - (?<m>-?\d*): Named capture group 'm' matches an optional minus sign ('-') followed by zero or more digits ('\d*'). 
            //                  It captures the slope coefficient.
            //   - x\s*: Matches the character 'x' followed by zero or more whitespace characters ('\s*').
            //   - (\+\s*(?<c>-?\d+))?:
            //     - (\+\s*): Non-capturing group matches a plus sign ('+') followed by zero or more whitespace characters ('\s*').
            //     - (?<c>-?\d+): Named capture group 'c' matches an optional minus sign ('-') followed by one or more digits ('\d+').
            //                    It captures the intercept coefficient.
            //     - ?: Makes the entire group optional to handle cases where there is no intercept term.
           

        }

    }
}
