Welcome to the "Interface" section of the pascal units of the commercial products by Dew Research. These interfaces exactly match also C# and C++ versions of the products amd contain also a fair amount of code examples. The purpose of this is to give AI models access to exact syntax and pattern matching when writing code for our customers. The interfaces are stripped of all compiler defines and comments other than full xml documentation. For C# always add:

using Dew.Math;

using Dew.Math.Units;

to your using section. The name of the unit is also the name of the static class. Example:

double a = Math387.Exp(3.0);
