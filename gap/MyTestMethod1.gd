



                  
#DeclareOperation("PureBettiTable",
          #        [ IsList ] );
#DeclareOperation( "PureTheGreatestNumber",
 #                 [ IsMatrix ]);
#DeclareOperation( "MatrixAfterSubstruction",
            #   [ IsMatrix ]);

#################################
##
## Global
##
#################################

DeclareGlobalVariable( "BOIJ_SOEDERBERG" );

#################################
##
## Categories
##
#################################

DeclareCategory( "IsDegreeSequence", IsObject );
DeclareCategory( "IsUpperEquationOfFacet", IsObject );
DeclareCategory( "IsLowerEquationOfFacet", IsObject );
DeclareCategory( "IsRootSequence", IsObject );
DeclareCategory( "IsFunctionalOfSheaf", IsObject );
DeclareCategory( "IsTruncatedFunctionalOfSheaf", IsObject );
DeclareCategory( "IsVirtualBettiTable", IsObject );


#
# Testtt: this is nothing more than a test of the packageMaker 
#
# Declarations
#

#! @Description
#!   Insert documentation for you function here, this is default file and was written by Max.

#! @Description
#! Resturns a list with integers
#! from a to b
#! @Returns IsList
#! @Arguments a,b
#DeclareGlobalFunction( "RangeOfIntegers" ,[IsInt, IsInt] );

DeclareGlobalFunction( "Testtt_Example" );

DeclareGlobalFunction( "FunctionalOfCohomologyTableInSpecificPosition" );

DeclareGlobalFunction( "TruncatedFunctionalOfCohomologyTableInSpecificPosition" );

DeclareGlobalFunction( "Minimum_for_Decomposition" );

DeclareGlobalFunction( "DecomposeCohomologyTable" );

DeclareGlobalFunction( "UpperBoundOfCohomologyTable" );

DeclareGlobalFunction( "VirtualPureBettiTable" );

DeclareGlobalFunction( "DegreeSequence" );

DeclareGlobalFunction( "RootSequence" );

DeclareGlobalFunction( "UpperEquationOfFacet" );

DeclareGlobalFunction( "LowerEquationOfFacet" );

DeclareGlobalFunction( "FunctionalOfSheaf" );

DeclareGlobalFunction( "TruncatedFunctionalOfSheaf" );

DeclareGlobalFunction( "ConvertToVirtualBettiTable" );

DeclareGlobalFunction( "LowerBoundOfBettiTable" );

DeclareGlobalFunction( "Betta" );

DeclareGlobalFunction( "CoefficientOfLowerBoundVirtualTable" );

DeclareGlobalFunction( "DecomposeBettiTable");

DeclareGlobalFunction( "NumberOfDigitsOfTheNumber" );

DeclareGlobalFunction( "Lcmm");

DeclareGlobalFunction( "GreatestCommonDivisor");

DeclareGlobalFunction( "Factor");

#################################

DeclareOperation( "IsIncreasingSequence",
               [ IsList]);
               
DeclareOperation( "AddLinesToPureBettiTable",
                [IsList,IsInt,IsInt]);
                
DeclareOperation( "UpperEquation",
                [IsList,IsInt,IsInt,IsInt]);
                
DeclareOperation( "UpperEquation",
                [IsDegreeSequence,IsInt,IsInt,IsInt]);
               
DeclareOperation( "LowerEquation",
                [IsList,IsInt,IsInt,IsInt]);
                
DeclareOperation( "LowerEquation",
                [IsDegreeSequence,IsInt,IsInt,IsInt]);
                
DeclareOperation("FirstNoneZeroElementInTheMatrix",
                       [IsMatrix]);
DeclareOperation( "FindLess",
                [IsList]);
                
DeclareOperation( "FindMore",
                [IsList]);
                              
DeclareOperation( "homalgCreateDisplayStringModified",
                 [IsBettiTable] );


#DeclareOperation( "FunctionOfThePureCohomolgyTable",
 #                 [IsList, IsInt, IsInt]); 
#DeclareOperation("CohomologyTally",
 #                [IsList,IsInt,IsInt]);                       
 
DeclareOperation( "PrintTheMatrixAsCohomologyTally",
               [ IsMatrix,IsInt ]);
#DeclareOperation("Pr",
 #                [IsList,IsInt,IsInt]);
#DeclareOperation("Pe",
 #                [IsList,IsInt,IsInt,IsInt,IsInt]);
                   

DeclareOperation("ReturnCorenrValues", [IsList, IsList, IsInt]);

DeclareOperation("FindMinimumForChomologyDecomposition", [IsList, IsList, IsList, IsInt]);

DeclareOperation("CornerPositions", [IsList]);

DeclareOperation("AddZerosToRows", [IsList]);

DeclareOperation("RemoveZerosFromRows", [IsList]);

DeclareOperation("ReconstructDegreeSequenceFromRootSequence", [IsList, IsInt]);

DeclareOperation("ReconstructDegreeSequenceFromRootSequence", [IsRootSequence, IsInt]);

DeclareOperation("UpperEquation",  [IsRootSequence,IsInt,IsInt,IsInt]);

DeclareOperation("TruncatedFunctionalOfPureResolutionInSpecificPosition", [IsDegreeSequence, IsInt, IsInt, IsInt, IsInt]);

DeclareOperation( "BettiNumbersOfDegreeSequence", [IsList]);

DeclareOperation( "BettiNumbersOfDegreeSequence", [IsDegreeSequence]);

DeclareOperation("IsDecreasing" , [IsList] );

DeclareOperation("FirstDecreasingPart" , [IsList] );

DeclareOperation("NumberOfSubsequencesOfConsecutiveIntegers" , [IsList] );

DeclareOperation("RankOfStandardSheafWithRootSequence", [IsList]);

DeclareOperation("RankOfStandardSheafWithRootSequence", [IsRootSequence]);

DeclareOperation("VHilbertPolynomial", [IsList, IsInt]);

DeclareOperation("CohomologyInSpecificPosition", [IsList, IsInt, IsInt]);

DeclareOperation("VirtualCohomologyTable",[IsList,IsInt, IsInt]);

DeclareOperation("VirtualCohomologyTable",[IsRootSequence,IsInt, IsInt]);

DeclareOperation("CohomologyTableOfStandardSheafwithRootSequence",[IsRootSequence,IsInt, IsInt]);

DeclareOperation("FunctionalOfCohomologyTableAsMatrix",[IsBettiTable, IsInt, IsInt]);

DeclareOperation("TruncatedFunctionalOfCohomologyTableAsMatrix",[IsBettiTable, IsInt, IsInt,IsInt, IsInt]);

DeclareOperation("FunctionalOfCohomologyTable",[IsBettiTable, IsInt, IsInt]);

DeclareOperation("TruncatedFunctionalOfCohomologyTable",[IsBettiTable, IsInt, IsInt,IsInt, IsInt]);

DeclareOperation("GreatestCommonDivisorOfMatrix",
                      [IsMatrix]);

DeclareOperation("MaximumOfTheMatrix",[IsMatrix]);

DeclareOperation("MinimumOfTheMatrix",[IsMatrix]);

DeclareOperation( "LowerBoundOfMatrix",
                  [ IsMatrix ] );
                  
DeclareOperation("PTM",
                  [ IsMatrix ] );
                  
DeclareOperation("LowestCommonMultipleOfTheList",
                  [ IsList ] );

DeclareOperation("PureBettiTableAsMatrix",
                  [ IsList ] );

DeclareOperation("PrintTheMatrixAsBettiTable",
                  [ IsMatrix,IsInt ] );
DeclareOperation("AddToXX",
                  [IsList,IsInt]);

DeclareOperation("\*",
                   [IsVirtualBettiTable,IsRat]);

DeclareOperation("\*",
                   [IsRat, IsVirtualBettiTable]);

DeclareOperation("\+",
                   [IsVirtualBettiTable,IsVirtualBettiTable]);
DeclareOperation("AddEmptyLinesToTheMatrix",
                 [IsMatrix, IsInt, IsInt]);

DeclareOperation("AddEmptyColumnsToTheMatrix",
                 [IsMatrix, IsInt]);

DeclareOperation("\*",
                   [IsUpperEquationOfFacet, IsRat]);
DeclareOperation("\*",
                   [IsRat, IsUpperEquationOfFacet]);
DeclareOperation("\*",
                   [IsLowerEquationOfFacet, IsRat]);
DeclareOperation("\*",
                   [IsRat, IsLowerEquationOfFacet]);