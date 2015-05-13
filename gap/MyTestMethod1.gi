
#################################
##
## Representations
##
#################################

DeclareRepresentation( "IsDegreeSequenceRep",
                       IsDegreeSequence and IsAttributeStoringRep,
                       [ "StrictlyIncreasingIntegerSequence" ] );

DeclareRepresentation( "IsUpperEquationOfFacetRep",
                       IsUpperEquationOfFacet and IsAttributeStoringRep,
                       [ "MatrixWithCoefficientsOfTheUpperEquation", "Grades" , "IsForRootSequence", "IsForDegreeSequence"] );

DeclareRepresentation( "IsLowerEquationOfFacetRep",
                       IsLowerEquationOfFacet and IsAttributeStoringRep,
                       [ "MatrixWithCoefficientsOfTheLowerEquation", "Grades" ] );
                       
DeclareRepresentation( "IsRootSequenceRep",
                       IsRootSequence and IsAttributeStoringRep,
                       [ "StrictlyDecreasingIntegerSequence" ] );

DeclareRepresentation( "IsFunctionalOfSheafRep",
                       IsFunctionalOfSheaf and IsAttributeStoringRep,
                       [ "MatrixWithCoefficientsOfTheFunctional", "SmallestTwist" ] );                      

DeclareRepresentation( "IsTruncatedFunctionalOfSheafRep",
                       IsTruncatedFunctionalOfSheaf and IsAttributeStoringRep,
                       [ "MatrixWithCoefficientsOfTheTruncatedFunctional", "SmallestTwist" ] );
                       
DeclareRepresentation( "IsVirtualBettiTableRep",
                       IsVirtualBettiTable and IsAttributeStoringRep,
                        [ "Object","IsBetti", "IsCohomology"] );
                       


##################################
##
## Family and Type
##
##################################
BindGlobal( "TheBoijSoederbergFamily",
  NewFamily( "TheBoijSoederbergFamily", IsObject ) );

BindGlobal( "TheTypeDegreeSequence",
  NewType( TheBoijSoederbergFamily,
                     IsDegreeSequenceRep ) );

BindGlobal( "TheTypeUpperEquationOfFacet",
  NewType( TheBoijSoederbergFamily,
                     IsUpperEquationOfFacet ) );

BindGlobal( "TheTypeLowerEquationOfFacet",
  NewType( TheBoijSoederbergFamily,
                     IsLowerEquationOfFacet ) );

BindGlobal( "TheTypeRootSequence",
  NewType( TheBoijSoederbergFamily,
                     IsRootSequenceRep ) );
                     
BindGlobal( "TheTypeFunctionalOfSheaf",
  NewType( TheBoijSoederbergFamily,
                     IsFunctionalOfSheafRep ) );
                     
BindGlobal( "TheTypeTruncatedFunctionalOfSheaf",
  NewType( TheBoijSoederbergFamily,
                     IsTruncatedFunctionalOfSheafRep ) );
                     
BindGlobal( "TheTypeVirtualBettiTable",
  NewType( TheBoijSoederbergFamily,
                     IsVirtualBettiTableRep ) );                     
                     

########################################################################################
##
InstallMethod( LowerBoundOfMatrix,
               [ IsMatrix],
               
  function( matrix )
    local i,j, list_of_rows;
    
    if Length( matrix ) = 0 or Length( matrix[ 1 ] ) = 0 then
        
        return [ ];
        
    fi;
    
    list_of_rows := [];
    
    for j in [ 1 .. Length( matrix[ 1 ] ) ] do
        
        for i in [ 1 .. Length( matrix ) ] do
            
            if not matrix[ i ][ j ] = 0 then
                
                Add( list_of_rows, j+i-2 );
                
                break;
                
            fi;
            
        od;
        
    od;
    
    return list_of_rows;
    
end );

#########################################################################################################################

InstallGlobalFunction( LowerBoundOfBettiTable,
function(arg)
local m, a,l;

if Length(arg)<> 1 then Error("The function should have only one arguments, Betti table or Virtual Betti table");fi;

if not IsBettiTable(arg[1]) and not IsVirtualBettiTable(arg[1]) then Error("The first argument should be Betti table or Virtual Betti table");fi;


if IsBettiTable(arg[1]) then m:= arg[1]!.matrix; a:= arg[1]!.row_range[1]; else m:= arg[1]!.Object!.matrix; a:= arg[1]!.Object!.row_range[1]; fi;
l:= LowerBoundOfMatrix(m);

return DegreeSequence( List([1..Length(l)], i->l[i]+a));

end );
######################################################################################################
# gives back the Betti number in specific position 
InstallGlobalFunction( Betta,

function(arg)
local betti_table, i, j;

if not Length( arg )=3 then Error("function should have 3 arguments, Betti table and i, j"); fi;

if IsBettiTable( arg[1] ) then  betti_table:= arg[1]; else betti_table:= arg[1]!.Object; fi;
i:= arg[2];
j:= arg[3];

return betti_table!.matrix[j-i-betti_table!.row_range[1]+1][i+1];

end );
          
######################################################################################################

InstallMethod( PureBettiTableAsMatrix,
               [ IsList ],
               
  function( list )
    local i,j,e,elist,clist,mat,m;
     
     if (IsIncreasingSequence(list)=false) then Error("The sequence should be increased"); else
     elist:=[];clist:=[];
      
    for i in [1..Length(list)] do
     
        e:=1;
     
        for j in [1..Length(list)] do
            
            if (j<>i) then e:=e*(list[j]-list[i]);fi;
           
        od;
          
        if ( e < 0 ) then e:=-1*e;fi;
        
        Add( elist,e);
        
    od;
    
    for i in [1..Length(list)] do
      
        Add(clist,LowestCommonMultipleOfTheList(elist)/elist[i]);
        
    od;
    
   m:=list[Length(list)]-Length(list)-list[1]+2;
    
   mat:= List([1..m],i->List([1..Length(list)],j->0));
    
   for i in [1..Length(list)] do
      
      mat[list[i]-i-list[1]+2][i]:=clist[i];
     
   od;
return mat;
fi;
end);

######################################################################################################

## this is just for printing upper and lower equations 
InstallMethod( PrintTheMatrixAsBettiTable,
               [ IsMatrix,IsInt ],
              

function(mat,n)
local m,i,t,j,v;
    m:=NumberOfDigitsOfTheNumber( MaximumOfTheMatrix(mat));

    v:=Maximum(NumberOfDigitsOfTheNumber(n+Length(mat)),NumberOfDigitsOfTheNumber(n));
    for i in [1..v+1] do
        
          Print(" ");
         
    od;
      
     for i in [1..Length(mat[1])] do
        
        Print(i-1);
        
        for t in [1..m+3-NumberOfDigitsOfTheNumber(i)] do
           
           Print(" ");
           
        od;
        
     od;
      
      Print("\n");
      
      for i in [1..NumberOfDigitsOfTheNumber(n+Length(mat))-1] do
          
           Print(" ");
          
      od;
       
     for i in [1..(m+3)*Length(mat[1])] do
        
        Print("-");
        
     od;
      
      Print("\n");
      
     for i in [1..Length(mat)] do
          
           for t in [1..v-NumberOfDigitsOfTheNumber(n+i-1)] do
               
               Print(" ");
              
           od;
            
           Print(n+i-1,"|");
           ##if(NumberOfDigitsOfTheNumber(n+i-1)=1) then Print(n+i-1," |");fi;
           
           ##if(NumberOfDigitsOfTheNumber(n+i-1)=2) then Print(n+i-1,"|");fi;
            
           for j in [1..Length(mat[1])] do
                  
                  if (mat[i][j]=0) then Print(".");else Print(mat[i][j]);fi;
                  
                  for t in [1..m+3-NumberOfDigitsOfTheNumber(mat[i][j])] do
                     
                     Print(" ");
                     
                  od;
                
           od;
        
     Print("\n");
     
      od;
     
end);

##
#InstallMethod( PureBettiTable,
          #     [ IsList],
         #     

    #function(list)
    #  
     # if (IsIncreasingSequence(list)=false) then Error("The sequence should be increased"); else 
    #  PrintTheMatrixAsBettiTable(PureBettiTableAsMatrix(list),list[1]);fi;
#end);


##
#InstallMethod( PureTheGreatestNumber,
 #              [ IsMatrix],
              

  #  function(mat)
   #local b,i,j,e;
   #
    #b:=PureBettiTableAsMatrix(LowerBoundOfMatrix(mat));
     
    #e:= mat[1][1]/b[1][1];j:=1;
    #for i in LowerBoundOfMatrix(mat) do
        
     #   if(e>mat[i-j+2][j]/b[i-j+2][j]) then e:=mat[i-j+2][j]/b[i-j+2][j];fi;
      #    
        # j:=j+1;
       # 
  #  od;
   # return e;#
#end);

######################################################################################################


InstallGlobalFunction( CoefficientOfLowerBoundVirtualTable,

function(arg)
local table, l, V, e, j,i;

table:= arg[1];

l:= LowerBoundOfBettiTable( table )!.StrictlyIncreasingIntegerSequence;

V:= VirtualPureBettiTable( l );

e:= Betta(table, 0, l[1])/Betta( V, 0, l[1] );

j:= 0;

for i in l do

   if e >  Betta(table, j, i)/Betta( V, j, i ) then e:= Betta(table, j, i)/Betta( V, j, i );fi;

   j:= j+1;
 
od;

return e;

end );

#InstallMethod( MatrixAfterSubstruction,
             #  [ IsMatrix],
              

  #  function(mat)
    
  #  local b,c,i,j;
        
  #   b:=PureBettiTableAsMatrix(LowerBoundOfMatrix(mat));
      
  #   c:=List([1..Length(mat)],i->List([1..Length(mat[1])],j->0));
     
   #  for i in [1..Length(b)] do
      
    #    for j in [1..Length(b[1])] do
           
  #         c[i][j]:=b[i][j];
           
    #    od;
       
   #  od;
     
   #  c:=mat-PureTheGreatestNumber(mat)*c;
     # return c;
#end);

######################################################################################################

InstallGlobalFunction(DecomposeBettiTable,
            
       function(arg)
       local table, l;
          
          
       if not IsBettiTable( arg[1] ) and not IsVirtualBettiTable( arg[1] ) then Error(" The argument should be a Betti table or a Virtual Betti table "); fi;
          
       if IsBettiTable( arg[1] ) then table:= ConvertToVirtualBettiTable( arg[1] , true, false ); else table:= arg[1];fi; 
          
          l:=[];
          
          while ( IsZero( table!.Object!.matrix )=false) do 

          Add(l,[CoefficientOfLowerBoundVirtualTable(table),LowerBoundOfBettiTable(table)!.StrictlyIncreasingIntegerSequence ]);
          
          table := table + (-1*CoefficientOfLowerBoundVirtualTable(table))* VirtualPureBettiTable( LowerBoundOfBettiTable(table) );

          od;
          
          return l;
end);

######################################################################################################

InstallMethod(AddLinesToPureBettiTable,
              [IsList,IsInt,IsInt],
           
              function(list,m,n)
              
              local i,x,mat;
              
              mat:=PureBettiTableAsMatrix(list);
              
              x:=List([1..n-m+1],i->List([1..Length(mat[1])],j->0));
              
             for i in [1..Length(list)] do
      
             x[list[i]-i-list[1]+2+list[1]-m][i]:=mat[list[i]-i-list[1]+2][i];
              
             od;

              PrintTheMatrixAsBettiTable(x,m);
#return x;
end);

######################################################################################################

InstallMethod(FindLess,
              [IsList],
      function(list)
      local j,v;
      
      v:=1;j:=Length(list);
      
      while(v=1) do
      
      if(list[j]>list[j-1]+1) then return j;break;fi;
           
           j:=j-1;
           
      if(j=1) then break;fi;
       
      od;
      
      return j;
end);

########################################################################################################
InstallMethod(FindMore,
              [IsList],
      function(list)
      local j,v;
      
      v:=1;j:=1;
      
      while(v=1) do
      
      if(list[j]<list[j+1]-1) then return j;break;fi;
       
      j:=j+1;
      
      if(j=Length(list)) then break;fi;
      
      od;
      
      return j;
end);
#######################################################################################################################################

#                                                             1         2           3       i+1     n+1  column index as matrix
#
#
#                                                      |      0         1           2       i        n   column index in Betti table
#                                         ____________ |_____________________________________________________________________________
#  row index as matrix        row index as Betti table |
#       1                               m              |         
#       2                               m+1            | 
#       3                               m+2            | 
#       .                               .              |
#       .                               .              |
#       i         --- +(m-1) --->       i+m-1          |         
#       .                               .              |
#       d-i+1-m   <--- +(1-m)----       d-i            |                                  B_(i,d)                                
#       .                               .              |          

InstallMethod(UpperEquation,
              [IsList,IsInt,IsInt,IsInt],
           
              function(d,r,m,n)
              
              local i,x,mat,t,s,k,y, L;
              
              L:= List([1..Length(d)],i->d[i]);
              
              L[r+2]:=L[r+2] -1;
              
              mat:=PureBettiTableAsMatrix(L);
              
              x:=List([1..n-m+1],i->List([1..Length(mat[1])],j->0));
              
              #y:=GreatestCommonDivisor(mat[list[r+1]-r-1-list[1]+2][r+1],mat[list[r+2]-r-2-list[1]+2][r+2]);
              
              x[L[r+2]-r-m][r+2]:=-mat[L[r+1]-r+1-L[1]][r+1];
               
              x[L[r+1]-r+1-m][r+1]:=mat[L[r+2]-r-L[1]][r+2];
              
              while(L<>[m..m+Length(L)-1]) do
              t:=FindLess(L);
              
              L[t]:=L[t]-1;
              
             mat:=PureBettiTableAsMatrix(L);
              
             s:=0;
             
             for i in [1..Length(L)] do
              
              if (i<>t) then s:=s+x[L[i]-i+2-m][i]*mat[L[i]-i-L[1]+2][i];fi;
              
             od;
              
             x[L[t]-t+2-m][t]:=-s/mat[L[t]-t-L[1]+2][t];
              
             od;
            
            #return x;
            # PrintTheMatrixAsBettiTable(x,m);
             return UpperEquationOfFacet(x, m, false, true);
end);

InstallMethod(UpperEquation,
              [IsDegreeSequence,IsInt,IsInt,IsInt],
           
    function(d,r,m,n)
    
    return UpperEquation(d!.StrictlyIncreasingIntegerSequence, r,m,n);
    
    end);

########################################################################################################           
              
InstallMethod(LowerEquation,
              [IsList,IsInt,IsInt,IsInt],
           
              function(d,r,m,n)
              
              local i,x,mat,t,s,k,y,L;
              
              L:= List([1..Length(d)],i->d[i]);
              
              L[r+1]:=L[r+1]+1;
              
              mat:=PureBettiTableAsMatrix(L);
              
              x:=List([1..n-m+1],i->List([1..Length(mat[1])],j->0));
              
               #y:=GreatestCommonDivisor(mat[list[r+1]-r-1-list[1]+2][r+1],mat[list[r+2]-r-2-list[1]+2][r+2]);
              
              x[L[r+2]-r-m][r+2]:=-mat[L[r+1]-r-1-L[1]+2][r+1];
               
              x[L[r+1]-r+1-m][r+1]:=mat[L[r+2]-r-2-L[1]+2][r+2];
              
              while(L<>[n..n+Length(L)-1]) do
              t:=FindMore(L);
              
              L[t]:=L[t]+1;
              
             mat:=PureBettiTableAsMatrix(L);
              
             s:=0;
             
             for i in [1..Length(L)] do
              
              if (i<>t) then s:=s+x[L[i]-i-L[1]+2+L[1]-m][i]*mat[L[i]-i-L[1]+2][i];fi;
              
             od;
              
             x[L[t]-t-L[1]+2+L[1]-m][t]:=-s/mat[L[t]-t-L[1]+2][t];
              
             od;
              
             ## maybe m should be changed
return LowerEquationOfFacet(x, m);
end);

######################################################################################################

InstallMethod(LowerEquation,
              [IsDegreeSequence,IsInt,IsInt,IsInt],
           
    function(d,r,m,n)
    
    return LowerEquation(d!.StrictlyIncreasingIntegerSequence, r,m,n);
    
    end);
    
######################################################################################################    

InstallGlobalFunction(VirtualPureBettiTable,
  function( arg )
    local matrix, min, row_range, col_range, list;
    
    if Length(arg)>1 then Error(" There should be only one argument, list or degree sequence"); fi;
    
    list:= arg[1];
    
    if IsList(list) then
    
             matrix := PureBettiTableAsMatrix(list);
    
             min := list[1];
    fi;
    
    if IsDegreeSequence(list) then
    
          matrix := PureBettiTableAsMatrix(list!.StrictlyIncreasingIntegerSequence);
    
          min := list!.StrictlyIncreasingIntegerSequence[1];
    fi;
    
    row_range := [ min .. min + Length( matrix ) - 1 ];
    
    col_range := [ 0 .. Length( matrix[1] ) - 1 ];

    return ConvertToVirtualBettiTable( HomalgBettiTable( matrix, row_range, col_range, rec( string := "" ) ), true, false);
    
end );

######################################################################################################
#InstallMethod(FunctionOfThePureCohomolgyTable,
 #             [IsList,IsInt, IsInt],

  #function(list,i,j)
   # local v,k;

    #v:=1;
    
    #for k in list do
    
   # v:= v*( j-k );
    
    #od;
     
    #if ( v<0 ) then v:=-v;fi;

    #if (i=0) then if (j>list[1]) then return v; else return 0; fi;fi;
    #if (i=Length(list)) then if ( j<list[Length(list)] ) then return v; else return 0; fi;fi;
    #if (0<i and i<Length(list)) then if ( list[i]>j and j>list[i+1] ) then return v; else return 0; fi;fi;
    #if (i<0 or i>Length(list)) then return 0;fi;

#end );

    
#InstallMethod(CohomologyTally,
 #             [IsList,IsInt,IsInt],

  # function (list,u,v)
   #    local 
    #    w,m,i,j,y,z;
     #   m:= Length(list)+1;
      #  w:= List([1..m],i->List([1..v-u+1],j->0));
       # for j in [1..m] do
        #   for i in [1..v-u+1] do
         #    y:=m-j;z:=-m+j+i+u-1;
          # w[j][i]:=FunctionOfThePureCohomolgyTable(list,y,z);
           #Print(m-j,",",-m+j+i+u-1,"   ");
          # od;
         #Print("\n");
     #   od;  
      #  return w/GreatestCommonDivisorOfMatrix(w);
   #end );
##
## this is just for printing pure CohomologyTally

######################################################################################################

InstallMethod( PrintTheMatrixAsCohomologyTally,
               [ IsMatrix,IsInt ],
              

function(mat,n)
local m,i,t,j,v;
    m:=NumberOfDigitsOfTheNumber( MaximumOfTheMatrix(mat));

    v:=NumberOfDigitsOfTheNumber(Length(mat)-1);
    for i in [1..v+1] do
        
          Print(" ");
         
    od;
      
     for i in [1..Length(mat[1])] do
        
        Print(n+i-1);
        
        for t in [1..m+3-NumberOfDigitsOfTheNumber(n+i-1)] do
           
           Print(" ");
           
        od;
        
     od;
      
      Print("\n");
      
      for i in [1..v] do
          
           Print(" ");
          
      od;
       
     for i in [1..(m+3)*Length(mat[1])] do
        
        Print("-");
        
     od;
      
      Print("\n");
      
     for i in [1..Length(mat)] do
          
           for t in [1..v-NumberOfDigitsOfTheNumber(Length(mat)-i)] do
               
               Print(" ");
              
           od;
            
           Print(Length(mat)-i,"|");
           ##if(NumberOfDigitsOfTheNumber(n+i-1)=1) then Print(n+i-1," |");fi;
           
           ##if(NumberOfDigitsOfTheNumber(n+i-1)=2) then Print(n+i-1,"|");fi;
            
           for j in [1..Length(mat[1])] do
                  
                  if (mat[i][j]=0) then Print(".");else Print(mat[i][j]);fi;
                  
                  for t in [1..m+3-NumberOfDigitsOfTheNumber(mat[i][j])] do
                     
                     Print(" ");
                     
                  od;
                
           od;
        
     Print("\n");
     
      od;
     
end);

#InstallMethod( Pr,
 #              [ IsList,IsInt,IsInt ],
              

#function(list,i,k)
#local j,s;
#s:=0;
#for j in [0..i] do 
#s:=s+FunctionOfThePureCohomolgyTable(list,j,-k)*(-1)^(i-j);
#od;
#return s;
#end);

##
#InstallMethod(Pe,
#              [IsList,IsInt,IsInt,IsInt,IsInt],
#
#function(list,t,c,i,l)
#local j,s;
#s:=0;
#if (i<t) then 
#for j in [0..i] do 
#s:=s+FunctionOfThePureCohomolgyTable(list,j,-i-l)*(-1)^(i-j);
#od;fi;

#if (i=t or i=t+1)and(l<c-t or l=c-t) then 
#for j in [0..t] do
#s:=s+FunctionOfThePureCohomolgyTable(list,j,-i-l)*(-1)^(i-j);
#od;fi;

#if (t=i or i=t+1)and(l>c-t) then 
#for j in [0..t-1] do
#s:=s+FunctionOfThePureCohomolgyTable(list,j,-i-l)*(-1)^(i-j);
#od;fi;

#if (t+1<i) then 
#for j in [0..i-2] do
#s:=s+FunctionOfThePureCohomolgyTable(list,j,-i-l)*(-1)^(i-j);
#od;fi;

#return s;
#end);

InstallMethod(AddToXX,
              [IsList,IsInt],
     function(list,n)
     local i,v,list2;
      list2:=List([1..Length(list)+1],i->0);
      v:=0;
     for i in list do 
     if n=i then v:=1;fi;
     od;
     if v=1 then return list;
     else 
         for i in [1..Length(list)] do
         list2[i]:=list[i];
         od;
         list2[Length(list)+1]:=n;return list2;fi;
    end);



##############################################
##
##  Constructors
##
##############################################

# this programs extend the matrix by adding empty lines in the top and below it, used to find the sum of two betti diagrams
InstallMethod(AddEmptyLinesToTheMatrix,
               [IsMatrix,IsInt,IsInt], 
function(matrix,a,b)
local newmatrix,i;

newmatrix:= List([1..Length(matrix)+a+b],i->List([1..Length(matrix[1])],j->0));

for i in [1..Length(matrix)] do 

        newmatrix[i+a]:= matrix[i];
od;
return newmatrix;

end );

# this programs extend the matrix by adding n empty columns to the right of the matrix, used to find the sum of two betti diagrams
InstallMethod(AddEmptyColumnsToTheMatrix,
               [IsMatrix, IsInt],

function(matrix, n)

return TransposedMat( AddEmptyLinesToTheMatrix( TransposedMat(matrix) , 0 , n )  );
end );

######################################################################################################

InstallGlobalFunction( DegreeSequence,
  function( arg )
    local  list, integer_sequence, degree_sequence;

     list:= arg[1];    
     if not IsList( list ) then

      Error( "the argument must be a list\n" );

    fi;

    if false in List( list, IsInt ) then

      Error( "the list must only contain integers\n" );

    fi;

    integer_sequence := Set( list ) ;
    
    degree_sequence := rec( StrictlyIncreasingIntegerSequence := integer_sequence );

    ObjectifyWithAttributes( degree_sequence, TheTypeDegreeSequence
                           );
    
    return degree_sequence;

end );

######################################################################################################

InstallGlobalFunction( RootSequence,
  function( arg )
    local  list, integer_sequence, root_sequence;

     list:= arg[1];    
     if not IsList( list ) then

      Error( "the argument must be a list\n" );

    fi;

    if false in List( list, IsInt ) then

      Error( "the list must only contain integers\n" );

    fi;

    integer_sequence := -1*Set( -1*list ) ;
    
    root_sequence := rec( StrictlyDecreasingIntegerSequence := integer_sequence );

    ObjectifyWithAttributes( root_sequence, TheTypeRootSequence
                           );
    
    return root_sequence;

end );

######################################################################################################

InstallGlobalFunction( UpperEquationOfFacet,
  function (arg)
    local equation;
    
    if not IsMatrix(arg[1]) then Error( "The first argument should be a matrix " ); fi;

    if not IsInt(arg[2]) then Error( "The second argument should be Integer " ); fi;

    if not Length(arg)=4 then Error( "The number of argument should be 4, the first is matrix and the second is integer "); fi;

    equation:= rec( MatrixWithCoefficientsOfTheUpperEquation:= arg[1], Grades:= arg[2], IsForRootSequence:=arg[3], IsForDegreeSequence:=arg[4]
                          );
  ObjectifyWithAttributes( equation, TheTypeUpperEquationOfFacet
                         );

return equation;
end );

######################################################################################################

InstallGlobalFunction( LowerEquationOfFacet,
  function (arg)
    local equation;
    
    if not IsMatrix(arg[1]) then Error( "The first argument should be a matrix " ); fi;

    if not IsInt(arg[2]) then Error( "The second argument should be Integer " ); fi;

    if not Length(arg)=2 then Error( "The number of argument should be 2, the first is matrix and the second is integer "); fi;

    equation:= rec( MatrixWithCoefficientsOfTheLowerEquation:= arg[1], Grades:= arg[2]
                          );
  ObjectifyWithAttributes( equation, TheTypeLowerEquationOfFacet
                         );

return equation;
end );

######################################################################################################

InstallGlobalFunction( FunctionalOfSheaf,
  function (arg)
    local equation;
    
    if not IsMatrix(arg[1]) then Error( "The first argument should be a matrix " ); fi;

    if not IsInt(arg[2]) then Error( "The second argument should be Integer " ); fi;

    if not Length(arg)=2 then Error( "The number of argument should be 2, the first is matrix and the second is integer "); fi;

    equation:= rec( MatrixWithCoefficientsOfTheFunctional:= arg[1], SmallestTwist:= arg[2] );
                  
  ObjectifyWithAttributes( equation, TheTypeFunctionalOfSheaf
                         );

return equation;
end );

######################################################################################################
InstallGlobalFunction( TruncatedFunctionalOfSheaf,
  function (arg)
    local equation;
    
    if not IsMatrix(arg[1]) then Error( "The first argument should be a matrix " ); fi;

    if not IsInt(arg[2]) then Error( "The second argument should be Integer " ); fi;

    if not Length(arg)=2 then Error( "The number of argument should be 2, the first is matrix and the second is integer "); fi;

    equation:= rec( MatrixWithCoefficientsOfTheTruncatedFunctional:= arg[1], SmallestTwist:= arg[2] );
                  
  ObjectifyWithAttributes( equation, TheTypeTruncatedFunctionalOfSheaf
                         );

return equation;
end );

######################################################################################################

InstallGlobalFunction( ConvertToVirtualBettiTable,
function(arg)
 local v_table;
 
 if not IsBettiTable(arg[1]) then Error("The first argument should be Betti table"); fi;
 
 if not IsBool(arg[2]) or not IsBool(arg[3]) then Error(" The second and third arguments should be boolian ");fi;
 
 if arg[2]=arg[3] then Error(" The second and third arguments should not be equal");fi;
 
 v_table:= rec( Object:=arg[1], IsBetti:=arg[2], IsCohomology:= arg[3] );
 
 ObjectifyWithAttributes( v_table, TheTypeVirtualBettiTable
                         );
 
return v_table;

end );
 
######################################################################################################

#these Betti numbers are uniqe up to positive multiple...
# We will use them to compute the truncated functional of a pure resolution, and exterior facet equation of type 2, in root sequences ...


#        i: 1 , 2  , 3  , ..., m+1
# coh_dim : m , m-1, m-2, ..., 0             so  (coh_dim = m -i +1) and ( i =  m - coh_dim +1 )
#                         
#        j:       1   2    3  ............ 
#        d:       a  a+1  a+2 ............   so (d = a+j-1) and (j= d-a+1)

InstallMethod(BettiNumbersOfDegreeSequence,
                       [IsList],
function(L)
local i,list_of_Betti_numbers, modificated_list,j,b, beta_0;

modificated_list:= List([1..Length(L)], j-> L[j]-L[1]);

list_of_Betti_numbers:= List([1..Length(L)], j-> 0);

beta_0 := 1;

for i in [2.. Length(L)] do

beta_0:= beta_0 * Binomial(modificated_list[i]-1, modificated_list[i]-modificated_list[i-1]-1);

                         od;
                         
list_of_Betti_numbers[1]:= beta_0;

for i in [2..Length(L)] do

     b:=1;
     
     for j in [2..Length(L)] do
     
         if i<>j then b:=b* (modificated_list[j])/(modificated_list[j]-modificated_list[i]); fi;    
         
                             od;
     
     list_of_Betti_numbers[i]:= AbsoluteValue(b)*beta_0;

                        od;

return list_of_Betti_numbers;

end );

InstallMethod(BettiNumbersOfDegreeSequence,
                       [IsDegreeSequence],
function(L)

return BettiNumbersOfDegreeSequence(L!.StrictlyIncreasingIntegerSequence);

end ); 

#####################################################################################################

# Here we actually don't need c, because what we are interrested is only when c= f_tau -1  ..... this porcedure can be made better ...

InstallMethod(TruncatedFunctionalOfPureResolutionInSpecificPosition, 
                     [IsDegreeSequence, IsInt, IsInt, IsInt, IsInt],
                    
function(L1, tau, c , cohomological_dimension, twist)
local found, position, Betti_list,i,L;

L:= L1!.StrictlyIncreasingIntegerSequence;

Betti_list:= BettiNumbersOfDegreeSequence(L1)/GreatestCommonDivisorOfMatrix([BettiNumbersOfDegreeSequence(L1)]);

found:= false;

if -1*twist in L then found:= true; position:= Position(L, -1*twist)-1;fi;

if found= false then return 0; fi;

if found = true and cohomological_dimension > position then return 0;fi;

if (found = true and cohomological_dimension = position) and position >= tau then return 0;fi;

if (found = true and cohomological_dimension = position) and position < tau then return Betti_list[position+1];fi;

if (found = true and cohomological_dimension = position-1) and position-1 >= tau then return 0;fi;

if (found = true and cohomological_dimension = position-1) and position-1 <  tau then return -1*Betti_list[position+1];fi;

if found = true and cohomological_dimension <= position-2 then  return ((-1)^(position - cohomological_dimension)) *Betti_list[position+1];fi;

end );

#####################################################################################################

# this gives the degree sequence from root sequences which define exterior facet of the fan of cohomology tables, this facet is of type 2....
# this will be used to find the facet equation with the help of the truncated functional in c, tau .... see the script....

InstallMethod( ReconstructDegreeSequenceFromRootSequence, 
                             [IsList, IsInt],
function(L, tau)
 local degree_sequence;
 
 degree_sequence:= List([1..Length(L)], i->L[i]);
 
 Add(degree_sequence, L[tau]+1, tau);
 Add(degree_sequence, L[tau]-1, tau+2);
 
 return DegreeSequence(-1*degree_sequence);
 
 end );
 
InstallMethod( ReconstructDegreeSequenceFromRootSequence, 
                             [IsRootSequence, IsInt],
function(L, tau)

return ReconstructDegreeSequenceFromRootSequence(L!.StrictlyDecreasingIntegerSequence, tau);

end );

######################################################################################################

InstallMethod(UpperEquation,
                 [IsRootSequence,IsInt,IsInt,IsInt],
function(R, tau, lower_twist, upper_twist)
local L,NrRows, NrColumns, c, d, matrix,i,j;
L:= R!.StrictlyDecreasingIntegerSequence;
NrRows:= Length(L)+1;
NrColumns:= upper_twist-lower_twist+1;
c:= L[tau]-1;
d:= ReconstructDegreeSequenceFromRootSequence(R, tau);

matrix:= List([1..NrRows], i-> List([1..NrColumns], j->0));

for i in [1..NrRows] do
for j in [1..NrColumns] do
matrix[i][j]:= TruncatedFunctionalOfPureResolutionInSpecificPosition(d, tau, c, NrRows -i, j+lower_twist-1 );
od;
od;

return UpperEquationOfFacet(matrix, lower_twist, true, false );
end );

#####################################################################################################
InstallMethod(IsDecreasing, 
                       [IsList],
function(L)

local i,logic_var;

logic_var:= true;

for i in [1..Length(L)-1] do
    
    if L[i] <= L[i+1] then logic_var:= false; fi;
    
                          od;
return logic_var;

end );

######################################################################################################
#LoadPackage("testtt");

InstallMethod( FirstDecreasingPart, 
                         [IsList],
function(L)
local i,counter, is_still_decreasing, L_rest,j;

L_rest:=List([1..Length(L)],j->L[j]);

if Length(L)=0 then return [0,[]]; fi;

if Length(L)=1 then return [1,[]]; fi;

counter:=1;

is_still_decreasing:=1;

i:=2;

while (is_still_decreasing =1 and not i> Length(L)) do

if L[i]=L[i-1]-1 then counter:=counter+1; i:=i+1; else is_still_decreasing:=0;fi;

                                                    od;

for i in [1..counter] do

Remove(L_rest,1);
                      od;
                        
return [counter, L_rest];

end );

#######################################################################################################

InstallMethod( NumberOfSubsequencesOfConsecutiveIntegers, 
                 [IsList],

function(L)
local M;
M:= [];

if FirstDecreasingPart(L)[2]=[] then return [FirstDecreasingPart(L)[1]]; fi;

Add(M, FirstDecreasingPart(L)[1]);

Append(M,NumberOfSubsequencesOfConsecutiveIntegers(FirstDecreasingPart(L)[2]));

return M;
                
end );

#######################################################################################################

InstallMethod(RankOfStandardSheafWithRootSequence,
                   [IsList],
function(L)

local n,m,i,Lx;

Lx:= NumberOfSubsequencesOfConsecutiveIntegers(L);

n:=Sum(Lx);

m:=1;

for i in [1..Length(Lx)] do

           m:= m* Factorial(Lx[i]);

                        od;

return Factorial(n)/m;

end );

InstallMethod(RankOfStandardSheafWithRootSequence,
                   [IsRootSequence],
function(L)

return RankOfStandardSheafWithRootSequence(L!.StrictlyDecreasingIntegerSequence);

end );
#########################################################################################################

InstallMethod(VHilbertPolynomial,
                           [IsList, IsInt],
function(L,d)
local x,i;
x:=1;
for i in [1..Length(L)] do
x:=x*(d-L[i]);
                        od;
return (1/Factorial(Length(L)))*AbsInt(x);
end );

########################################################################################################

InstallMethod(CohomologyInSpecificPosition,
                      [IsList,IsInt,IsInt],
function(RootSequence, row_index, column_index)
local rank,L;

L:=RootSequence;

if (row_index > Length(L) or row_index<0) then return 0;fi;

if row_index=0 then if column_index> L[1] then return VHilbertPolynomial(L,column_index); 

                    else return 0; 
                    
                    fi;
fi;

if row_index=Length(L) then if column_index< L[Length(L)] then return VHilbertPolynomial(L,column_index); 

                            else return 0; 
                              
                            fi;
fi;

if (column_index> L[row_index+1] and  column_index< L[row_index]) then return VHilbertPolynomial(L, column_index); else return 0; fi;
                    
end);

###############################################################################################

InstallMethod(VirtualCohomologyTable,
                     [IsList,IsInt, IsInt],
function(L,a1,b)

local row_range, column_range, matrix,i,j, a;


row_range:= Length(L)+1;

a:= a1- row_range+1;

column_range:= b-a+1;

matrix:= List([1..row_range],i->List([1..column_range],j-> 0)  );


for i in [1..row_range] do

    for j in [1..column_range] do

    matrix[i][j]:=CohomologyInSpecificPosition(L, row_range-i, j+a-1);

    od;
od;

return ConvertToVirtualBettiTable( HomalgBettiTable( RemoveZerosFromRows(matrix), -1*Set(-1*[0..Length(L)]), [a+row_range-1..b], HomalgCocomplex( HOMALG_MATRICES.ZZ ), false ), false, true);

end );

# VirtualCohomologyTable given a root sequence

InstallMethod(VirtualCohomologyTable,
                     [IsRootSequence,IsInt, IsInt],
function(L,a,b)

return VirtualCohomologyTable(L!.StrictlyDecreasingIntegerSequence,a,b);
end);

###############################################################################################

InstallMethod(CohomologyTableOfStandardSheafwithRootSequence,
                     [IsRootSequence,IsInt, IsInt],
function(L,a,b)
return RankOfStandardSheafWithRootSequence(L)*VirtualCohomologyTable(L,a,b);
end );

################################################################################################

InstallGlobalFunction( FunctionalOfCohomologyTableInSpecificPosition,
       # ( Betti table, cohomological_dimension , twist )
function(arg) 

local NrRows, lowest_twist,matrix_of_the_table,j, twist,s, cohomological_dimension;

if IsBettiTable(arg[1]) then 
			NrRows:= arg[1]!.NrRows;
			
			cohomological_dimension:= arg[2];
			
			twist:= -1*arg[3];
			
			matrix_of_the_table:= AddZerosToRows(arg[1]!.matrix);
			
			lowest_twist:= arg[1]!.column_range[1]-NrRows+1;

else 
			Error("The first argument should be Betti table, and the second and third are cohomological dimension and the twist");
fi;

if cohomological_dimension < NrRows then

                         s:=0;
                         
			 for j in [0..cohomological_dimension] do
			 
			          s:= s+ ((-1)^(cohomological_dimension-j))*matrix_of_the_table[NrRows-j][twist - lowest_twist+1];
			  
                         od;
else 

                        return -1*FunctionalOfCohomologyTableInSpecificPosition(arg[1],arg[2]-1,arg[3]);
                        
fi;

return s;
end );

###############################################################################################
                     
InstallGlobalFunction( TruncatedFunctionalOfCohomologyTableInSpecificPosition,
        #(Betti table, tau, f_tau , cohomological dimension, twist)
function(arg) 
local NrRows, lowest_twist,matrix_of_the_table,j, twist,s, cohomological_dimension,l, tau, c;

if IsBettiTable(arg[1]) then 

           tau:= arg[2];

           c:= arg[3];

           NrRows:= arg[1]!.NrRows;

           cohomological_dimension:= arg[4];

           twist:= arg[5];

           matrix_of_the_table:= AddZerosToRows(arg[1]!.matrix);

           lowest_twist:= arg[1]!.column_range[1]-NrRows+1;

           l:= twist-cohomological_dimension;

else 

   Error("The first argument should be Betti table");
  
fi;

if cohomological_dimension < tau   then return FunctionalOfCohomologyTableInSpecificPosition(arg[1],cohomological_dimension,twist);fi;

if (cohomological_dimension in [tau, tau+1] and l <= c-tau) then return ((-1)^(cohomological_dimension -tau))*FunctionalOfCohomologyTableInSpecificPosition(arg[1],tau,twist);fi;

if (cohomological_dimension in [tau, tau+1] and l > c-tau) then return  ((-1)^(cohomological_dimension -tau+1))*FunctionalOfCohomologyTableInSpecificPosition(arg[1],tau -1,twist) ;fi;

if cohomological_dimension > tau+1 then return FunctionalOfCohomologyTableInSpecificPosition(arg[1],cohomological_dimension-2,twist)  ;fi;

end );


#######################################################################################

InstallMethod(FunctionalOfCohomologyTableAsMatrix,

                                  [IsBettiTable, IsInt, IsInt],
                                  
   function(L, lower_bound, upper_bound)
   
   local functional_matrix, NrRows, NrColumns, i, j;

   NrRows:= upper_bound-lower_bound+1;
   
   NrColumns:= L!.NrRows + 1;

   functional_matrix:= List([1..NrRows],i-> List([1..NrColumns],j->0));
   
   for j in [1..NrRows] do
     
     for i in [1..NrColumns] do
     
          functional_matrix[j][i]:= FunctionalOfCohomologyTableInSpecificPosition(L, i-1, (lower_bound -1)+j +(i-1));
          
     od;
   
   od;
   
   return functional_matrix;
end );

#####################################################################################

InstallMethod(TruncatedFunctionalOfCohomologyTableAsMatrix,
                                  [IsBettiTable, IsInt, IsInt, IsInt, IsInt],
   function(L,tau, c ,lower_bound, upper_bound)
   local functional_matrix, NrRows, NrColumns, i, j;
   

   NrRows:= upper_bound-lower_bound+1;
   
   NrColumns:= L!.NrRows + 1;

   functional_matrix:= List([1..NrRows],i-> List([1..NrColumns],j->0));
   
   for j in [1..NrRows] do
     
     for i in [1..NrColumns] do
     
          functional_matrix[j][i]:= TruncatedFunctionalOfCohomologyTableInSpecificPosition(L, tau, c, i-1, (lower_bound -1)+j +(i-1));
          
     od;
   
   od;
   
   return functional_matrix;
end );

#####################################################################################

InstallMethod( FunctionalOfCohomologyTable, 
                 [IsBettiTable, IsInt, IsInt],
                 
  function(L, lower_bound, upper_bound)
  
  return FunctionalOfSheaf( FunctionalOfCohomologyTableAsMatrix(L, lower_bound, upper_bound), lower_bound );
  
  end );
  
#####################################################################################

InstallMethod( TruncatedFunctionalOfCohomologyTable, 
                 [IsBettiTable, IsInt, IsInt, IsInt, IsInt],
                 
  function(L,tau, c ,lower_bound, upper_bound)
  
  return TruncatedFunctionalOfSheaf( TruncatedFunctionalOfCohomologyTableAsMatrix(L,tau, c ,lower_bound, upper_bound), lower_bound );
  
  end );

#####################################################################################

InstallGlobalFunction (UpperBoundOfCohomologyTable,
      
function( arg )
local NrRows, NrColumns, i,j, found, upper_bound, matrix, smallest_twist ;

 if Length(arg)=2 and not IsBettiTable(arg[1]) and not IsVirtualBettiTable(arg[1]) then 
  
                matrix:= arg[1];
                
                smallest_twist:= arg[2];
               
  fi;
  
  if IsBettiTable(arg[1]) then 
  
                matrix:= AddZerosToRows(arg[1]!.matrix); 
                
                smallest_twist:= arg[1]!.column_range[1]-Maximum(arg[1]!.row_range);
                
  fi;
  
  if IsVirtualBettiTable(arg[1]) and arg[1]!.IsCohomology= true then 
  
                matrix:= AddZerosToRows(arg[1]!.Object!.matrix); 
                
                smallest_twist:= arg[1]!.Object!.column_range[1]-Maximum(arg[1]!.Object!.row_range);
                
  fi;

NrRows:= Length(matrix);
NrColumns:= Length(matrix[1]);

if matrix[NrRows][NrColumns]=0 or matrix[1][1]=0 then 
  
        Error( "Please make sure that the given table is cohomology table of a vector bundle, or maybe you should extend the twists range\n" );
fi;

for i in [2..NrRows] do

     if matrix[i][1] <> 0 then 

            Error( "Please make sure that the given table is cohomology table of a vector bundle, or maybe you should extend the twists range\n" );
     fi;

     if matrix[i-1][NrColumns] <> 0 then 

            Error( "Please make sure that the given table is cohomology table of a vector bundle, or maybe you should extend the twists range\n" );
     fi;
od;

upper_bound:= [];

for i in [1..NrRows-1] do

     found:= false;
     
     for j in [1..NrColumns] do

          if matrix[i][NrColumns-j+1]<>0 then found:= true; Add(upper_bound, smallest_twist +(NrColumns-j+1));break;fi;

     od;

     if found= false then Add(upper_bound, upper_bound[Length(upper_bound)]+1 );fi; 
od;


return List([1..NrRows-1], i-> upper_bound[NrRows-i]);
end );



######################################################################################

# this is important, this rebuild cohomology table!.matrix into a new matrix as the one see in screen...

InstallMethod( AddZerosToRows,
                   [IsList],

function(matrix)
local new_matrix, m,i,j;

new_matrix:= List([1..Length(matrix)],i-> List([1..Length(matrix[1])], j-> matrix[i][j]) );

m:= Length(matrix);

for i in [1..m] do 
     
       for j in [1..i-1] do
    
              Add(new_matrix[i],0,1);
       od;

       for j in [1..m-i] do
         
              Add(new_matrix[i], 0);

       od;
      
od;

return new_matrix;
end );

# this is important, this rebuild cohomology table as seen in screen to a new matrix as the one inside the rec of the table...

InstallMethod(RemoveZerosFromRows, 
                       [IsList],
 
function(matrix)

local new_matrix, m , i , j;

new_matrix:= List([1..Length(matrix)],i-> List([1..Length(matrix[1])], j-> matrix[i][j]) );

m:= Length(matrix);

for i in [1..m] do 
     
       for j in [1..i-1] do
    
              Remove(new_matrix[i],1);
       od;

       for j in [1..m-i] do
         
              Remove(new_matrix[i], Length(new_matrix[i]) );

       od;
      
od;

return new_matrix;
end );


######################################################################################

InstallMethod(CornerPositions,
                [IsList],
function(z)
local l,i;
l:= [];

for i in [1..Length(z)-1] do

     if z[i+1] < z[i]-1 then Add(l, [i,z[i]-1]); fi;
od;

Add(l,[Length(z), z[Length(z)]-1]);
return l;

end );

######################################################################################
#        i: 1 , 2  , 3  , ..., m+1
# coh_dim : m , m-1, m-2, ..., 0             so  (coh_dim = m -i +1) and ( i =  m - coh_dim +1 )
#                         
#        j:       1   2    3  ............ n 
#        d:       a  a+1  a+2 ............ a+n-1       so (d = a+j-1) and (j= d-a+1)
######################################################################################

InstallMethod(ReturnCorenrValues, 
                   [IsList, IsList, IsInt],
                   
function(matrix, corner_positions, smallest_twist)
  local list,l, cohomological_dimension, twist, value_in_the_old_matrix, value_in_the_new_matrix, m;
  
  m:= Length(matrix)-1;
  
  list:= [];
  for l in corner_positions do

         cohomological_dimension := l[1];

         twist:= l[2];

 Add(list, matrix[m-cohomological_dimension +1][twist -smallest_twist +1 ]);
od;
  
return list;

end );


######################################################################################
InstallGlobalFunction ( Minimum_for_Decomposition,

function(A, B)

 local i, d;
 
 d:= [];
 for i in [1.. Length(A)] do
 
          Add(d, A[i]/B[i]);
 
 od;
 
 return Minimum(d);
 
 end );
 
 ####################################################################################
 
 InstallGlobalFunction(DecomposeCohomologyTable,
                    
  function(arg)
  
  local z,c,gamma1, smallest_twist, s_gamma, gamma, L, NrRows, NrColumns, q, cv_gamma, cv_s_gamma , i, Re, Null_Matrix;
  
  if Length(arg)=2 and not IsBettiTable(arg[1]) and not IsVirtualBettiTable(arg[1]) then 
  
                gamma1:= arg[1];
                
                smallest_twist:= arg[2];
               
  fi;
  
  if IsBettiTable(arg[1]) then 
  
                gamma1:= AddZerosToRows(arg[1]!.matrix); 
                
                smallest_twist:= arg[1]!.column_range[1]-Maximum(arg[1]!.row_range);
                
  fi;
  
  if IsVirtualBettiTable(arg[1]) and arg[1]!.IsCohomology= true then 
  
                gamma1:= AddZerosToRows(arg[1]!.Object!.matrix); 
                
                smallest_twist:= arg[1]!.Object!.column_range[1]-Maximum(arg[1]!.Object!.row_range);
                
  fi;
  
  NrRows:= Length(gamma1);
  NrColumns:= Length(gamma1[1]);
  Re:= [];
  
  gamma:= List( [1..NrRows], i->List([1..NrColumns], j->gamma1[i][j]) );
  
  Null_Matrix:= List( [1..NrRows], i->List([1..NrColumns], j->0) );
  
  i:= 1;
  
  while gamma <> Null_Matrix and i< 1000 do
  
  z:= UpperBoundOfCohomologyTable(gamma, smallest_twist);
  
  s_gamma := AddZerosToRows(VirtualCohomologyTable(z, smallest_twist+NrRows -1, smallest_twist+ NrColumns-1)!.Object!.matrix);
  
  c:= CornerPositions(z);
  
  cv_gamma:=ReturnCorenrValues(gamma, c, smallest_twist);
  
  cv_s_gamma:= ReturnCorenrValues(s_gamma, c, smallest_twist);
  
  q:= Minimum_for_Decomposition(cv_gamma, cv_s_gamma);
  
  gamma:= gamma - q*s_gamma;
  
  Add(Re, [q, z]);
  
  i:= i+1;
  
  od;

  if i= 100 then 
   
   Error("It seems that the given table is not for a vector bundle, since it took a lot of time (more than 100 iteration) "); 
  
  fi;
  
  return Re;
 
  end );
  



######################################################################################
######################################################################################
######################################################################################


############################################
##
##  Display and View
##
############################################
 

InstallMethod( Display,
               "For a degree sequence of betti diagram",
               [ IsDegreeSequenceRep ],
               
  function( degree_sequence )

    Print( degree_sequence!.StrictlyIncreasingIntegerSequence );

end );

InstallMethod( ViewObj,
               "For a degree sequence of betti diagram",
               [ IsDegreeSequenceRep ],
               
  function( degree_sequence )

    Print( "< Degree seqeunce of virtual Betti table >" );

end );

InstallMethod( Display,
               "For a root sequence of virtual cohomology table",
               [ IsRootSequenceRep ],
               
  function( root_sequence )

    Print( root_sequence!.StrictlyDecreasingIntegerSequence );

end );

InstallMethod( ViewObj,
               "For a root sequence of virtual cohomology table",
               [ IsRootSequenceRep ],
               
  function( root_sequence )

    Print( "< Root sequence of virtual cohomology table >" );

end );

InstallMethod( Display,
               "for the Upper Equation Of Facet",
              [IsUpperEquationOfFacet],
  function( equation )
if equation!.IsForDegreeSequence= true then 

        PrintTheMatrixAsBettiTable(equation!.MatrixWithCoefficientsOfTheUpperEquation, equation!.Grades);
 
fi;

if equation!.IsForRootSequence= true then 
 
       PrintTheMatrixAsCohomologyTally(equation!.MatrixWithCoefficientsOfTheUpperEquation, equation!.Grades);
 
fi;
  end );


InstallMethod( Display,
               "for the Lower Equation Of Facet",
              [IsLowerEquationOfFacet],
  function( equation )

PrintTheMatrixAsBettiTable(equation!.MatrixWithCoefficientsOfTheLowerEquation, equation!.Grades);
  end );

InstallMethod( ViewObj,
               "for the Upper Equation Of Facet",
              [IsUpperEquationOfFacet],
  function( equation )
  Print("< Upper equation Of a facet >");
  end );


InstallMethod( ViewObj,
               "for the Lower Equation Of Facet",
              [IsLowerEquationOfFacet],
  function( equation )
  Print("< Lower equation Of a facet >");
  end );
  
InstallMethod( ViewObj,
               "for the functional of a cohomology table",
               [IsFunctionalOfSheaf],
  function( functional )
  Print( "< functional of cohomology table >");
  end ); 
  
InstallMethod( ViewObj,
               "for the functional of a cohomology table",
               [IsTruncatedFunctionalOfSheaf],
  function( functional )
  Print( "< Truncated functional of cohomology table >");
  end );
  
InstallMethod( Display,
               "for the functional of cohomology table",
              [IsFunctionalOfSheaf],
  function( functional )

PrintTheMatrixAsBettiTable(functional!.MatrixWithCoefficientsOfTheFunctional, functional!.SmallestTwist);

  end );
  
InstallMethod( Display,
               "for the truncated functional of cohomology table",
              [IsTruncatedFunctionalOfSheaf],
  function( functional )

PrintTheMatrixAsBettiTable(functional!.MatrixWithCoefficientsOfTheTruncatedFunctional, functional!.SmallestTwist);

  end );
  
 
InstallMethod( ViewObj,
               "for virtual Betti table",
               [IsVirtualBettiTable],
  function( table )
  local V;
  
    
  if table!.IsCohomology= true then 
  
                Print("< Virtual cohomology table >"); 
 
  else  
  
        Print("< Virtual Betti table >"); 
        
  fi;
 
end );
  
  
# This is copied and modified from mohamed's work to hide stars * in a virtual cohomology table of root sequence,

InstallMethod( homalgCreateDisplayStringModified,
        "for Betti diagrams",
        [ IsBettiTable ],
        
  function( o )
    local SpectralSequenceConvention, betti, row_range, column_range,
          higher_vanish, twist, EulerCharacteristic, reverse,
          nr_rows, nr_cols, total, max, twist_range, chi,
          MAX, display, ar, i, pos, marker;
    
    ## the spectral sequence convention for Betti diagrams
    SpectralSequenceConvention := o!.SpectralSequenceConvention;
    
    ## collect the relevant data from the diagram
    betti := MatrixOfDiagram( o );
    
    row_range := RowDegreesOfBettiTable( o );
    column_range := ColumnDegreesOfBettiTable( o );
    
    if IsBound( o!.higher_vanish ) then
        higher_vanish := o!.higher_vanish;
    fi;
    
    if IsBound( o!.twist ) then
        twist := o!.twist;
    fi;
    
    if IsBound( o!.EulerCharacteristic ) and
       IsUnivariatePolynomial( o!.EulerCharacteristic ) then
        EulerCharacteristic := o!.EulerCharacteristic;
    fi;
    
    ## read the row range upside down?
    reverse := o!.reverse;
    
    nr_rows := NrRows( o );
    nr_cols := NrColumns( o );
    
    ## now prepare constructing the display string
    
    ## the list of total dimensions
    total := ListWithIdenticalEntries( nr_rows, 1 ) * betti;
    
    ## save it
    o!.total := total;
    
    ## get the maximum width in the matrix
    max := MaximumList( List( betti, r -> MaximumList( List( r, a -> Length( String( a ) ) ) ) ) );
    max := Maximum( MaximumList( List( column_range, a -> Length( String( a ) ) ) ), max );
    max := Maximum( MaximumList( List( total, a -> Length( String( a ) ) ) ), max );
    
    if SpectralSequenceConvention then
        ar := column_range[1];
        if nr_rows > 1 then
            max := Maximum( MaximumList( List( [ ar - ( nr_rows - 1 ) .. ar - 1 ], a -> Length( String( a ) ) ) ), max );
        fi;
    fi;
    
    if IsBound( twist ) then
        twist_range := column_range - ( nr_rows - 1 );
        max := Maximum( MaximumList( List( twist_range, a -> Length( String( a ) ) ) ), max );
        if IsBound( EulerCharacteristic ) then
            
            if SpectralSequenceConvention then
                twist_range := [ twist_range[1] .. twist_range[nr_cols] + ( nr_rows - 1 ) ];
                chi := List( twist_range, i -> Value( EulerCharacteristic, i ) );
            else
                chi := List( twist_range + ( nr_rows - 1 ), i -> Value( EulerCharacteristic, i ) );
            fi;
            
            ## save it
            o!.Euler := chi;
            
            max := Maximum( MaximumList( List( chi, a -> Length( String( a ) ) ) ), max );
            
        elif twist = row_range[nr_rows] then ## we might have computed the syzygies up to some degree bound only
            
            chi := List( [ 1 .. nr_cols - twist ], j -> Sum( [ 0 .. nr_rows - 1 ], i -> (-1)^i * betti[nr_rows-i][i+j] ) );
            
            ## save it
            o!.Euler := chi;
            
            if IsBound( higher_vanish ) and column_range[Length( column_range )] >= higher_vanish - 1 then
                Append( chi, List( [ Maximum( nr_cols - twist + 1, 1 ) .. nr_cols ],
                        j -> Sum( [ 0 .. nr_rows - 1 ],
                                function( i )
                                  if IsBound( betti[nr_rows-i][i+j] ) then
                                      return (-1)^i * betti[nr_rows-i][i+j];
                                  else
                                      return 0;
                                  fi; end ) ) );
                                  
                ## save it
                o!.Euler := chi;
            else
                Append( chi, ListWithIdenticalEntries( twist, "?" ) );
            fi;
            
            max := Maximum( MaximumList( List( chi, a -> Length( String( a ) ) ) ), max );
            
        fi;
    fi;
    
    ## finally add a space
    max := max + 1;
    
    ## the maximum of the legend column
    MAX := MaximumList( List( row_range, a -> Length( String( a ) ) ) );
    
    if IsBound( twist ) then
        MAX := Maximum( MAX, Length( "twist" ) );
    else
        MAX := Maximum( MAX, Length( "twist" ) );
    fi;
    
    ## create the display string:
    
    display := "";
    
    if SpectralSequenceConvention then
        nr_cols := nr_cols + nr_rows - 1;
    fi;
    
    ## total:
    if nr_rows > 1 then
        Append( display, FormattedString( "total", MAX ) );
        Append( display, ": " );
        Perform( total, function( i ) Append( display, FormattedString( i, max ) ); end );
        if SpectralSequenceConvention then
            Perform( [ 1 .. nr_rows - 1 ], function( i ) Append( display, FormattedString( "?", max ) ); end );
        fi;
        Append( display, "\n" );
        if SpectralSequenceConvention then
            Append( display, ListWithIdenticalEntries( MAX + 2, '-' ) );
            Append( display, Flat( ListWithIdenticalEntries( nr_cols, Concatenation( ListWithIdenticalEntries( max - 1, '-' ), "|" ) ) ) );
        else
            Append( display, ListWithIdenticalEntries( MAX + 2 + nr_cols * max, '-' ) );
        fi;
        Append( display, "\n" );
    fi;
    
    ## twist:
    if IsBound( twist ) and nr_rows > 1 and not SpectralSequenceConvention then
        Append( display, FormattedString( "twist", MAX ) );
        Append( display, ": " );
        Perform( twist_range, function( i ) Append( display, FormattedString( i, max ) ); end );
        Append( display, "\n" );
        Append( display, ListWithIdenticalEntries( MAX + 2, '-' ) );
        Append( display, Flat( ListWithIdenticalEntries( nr_cols, Concatenation( ListWithIdenticalEntries( max - 1, '-' ), "|" ) ) ) );
        Append( display, "\n" );
    fi;
    
    if reverse then
        row_range := Reversed( row_range );
    fi;
    
    ## betti:
    if SpectralSequenceConvention then
        for ar in [ 1 .. nr_rows ] do
            Append( display, FormattedString( String( row_range[ar] ), MAX ) );
            Append( display, ": " );
            Perform( [ 1 .. ar - 1 ], function( i ) Append( display, FormattedString( ".", max ) ); end );
            for i in [ 1 .. nr_cols - ( nr_rows - 1 ) ] do
                if IsZero( betti[ar][i] ) then
                    Append( display, FormattedString( ".", max ) );
                else
                    Append( display, FormattedString( String( betti[ar][i] ), max ) );
                fi;
            od;
            if IsBound( higher_vanish ) and column_range[Length( column_range )] >= higher_vanish - 1 then
                Perform( [ 1 .. nr_rows - ar ], function( i ) Append( display, FormattedString( "0", max ) ); end );
            else
                Perform( [ 1 .. nr_rows - ar ], function( i ) Append( display, FormattedString( ".", max ) ); end );
            fi;
            Append( display, "\n" );
        od;
    else
        for ar in [ 1 .. nr_rows ] do
            Append( display, FormattedString( String( row_range[ar] ), MAX ) );
            Append( display, ": " );
            for i in [ 1 .. nr_cols ] do
                if IsZero( betti[ar][i] ) then
                    Append( display, FormattedString( ".", max ) );
                else
                    Append( display, FormattedString( String( betti[ar][i] ), max ) );
                fi;
            od;
            Append( display, "\n" );
        od;
    fi;
    
    ## degree/twist:
    if IsBound( twist ) then
        Append( display, ListWithIdenticalEntries( MAX + 2, '-' ) );
        if IsBound( higher_vanish ) then
            pos := Position( column_range, higher_vanish );
            if IsBound( o!.markers ) and IsList( o!.markers ) then
                marker := First( o!.markers, i -> i[1] = higher_vanish );
                if IsList( marker ) then
                    marker := marker[2];
                fi;
            fi;
        fi;
        
        if IsBound( pos ) and IsPosInt( pos ) then
            pos := pos + nr_cols - Length( column_range );
            Append( display, Flat( ListWithIdenticalEntries( pos - 1, Concatenation( ListWithIdenticalEntries( max - 1, '-' ), "|" ) ) ) );
            if not IsString( marker ) then
                marker := "V";
            fi;
            Append( display, Concatenation( ListWithIdenticalEntries( max - 1, '-' ), marker ) );
            Append( display, Flat( ListWithIdenticalEntries( nr_cols - pos, Concatenation( ListWithIdenticalEntries( max - 1, '-' ), "|" ) ) ) );
        else
            Append( display, Flat( ListWithIdenticalEntries( nr_cols, Concatenation( ListWithIdenticalEntries( max - 1, '-' ), "|" ) ) ) );
        fi;
        Append( display, "\n" );
        Append( display, FormattedString( "twist", MAX ) );
    else
        Append( display, ListWithIdenticalEntries( MAX + 2 + nr_cols * max, '-' ) );
        Append( display, "\n" );
        Append( display, FormattedString( "twist", MAX ) );
    fi;
    
    Append( display, ": " );
    
    if SpectralSequenceConvention then
        ar := column_range[1];
        Perform( [ ar - ( nr_rows - 1 ) .. ar - 1 ], function( i ) Append( display, FormattedString( i, max ) ); end );
    fi;
    
    Perform( column_range, function( i ) Append( display, FormattedString( i, max ) ); end );
    
    Append( display, "\n" );
    
    ## Euler characteristic:
    if IsBound( chi ) then
        Append( display, ListWithIdenticalEntries( MAX + 2 + nr_cols * max, '-' ) );
        Append( display, "\n" );
        Append( display, FormattedString( "Euler", MAX ) );
        Append( display, ": " );
        if SpectralSequenceConvention and not IsBound( EulerCharacteristic ) then
            Perform( [ 1 .. nr_rows - 1 ], function( i ) Append( display, FormattedString( "?", max ) ); end );
        fi;
        Perform( chi, function( i ) Append( display, FormattedString( i, max ) ); end );
        Append( display, "\n" );
    fi;
    
    return display;
    
end );

##
# this is modified to hide stars.... from supernatural cohomology table ...
InstallMethod( Display,
        "for Virtual Betti diagrams",
        [ IsVirtualBettiTable ],
        
  function( table )
    local SpectralSequenceConvention, o;
    
    o:= table!.Object;
    
    if table!.IsBetti = true then Display(o);fi;
    
    if table!.IsCohomology = true then 
    
    # the spectral sequence convention for Betti diagrams
    if IsBound( HOMALG.SpectralSequenceConventionForBettiTables ) then
        SpectralSequenceConvention := HOMALG.SpectralSequenceConventionForBettiTables = true;
    elif IsComplexOfFinitelyPresentedObjectsRep( o!.object ) and
      IsBound( HOMALG.SpectralSequenceConventionForBettiTablesOfComplexes ) then
        SpectralSequenceConvention := HOMALG.SpectralSequenceConventionForBettiTablesOfComplexes = true;
    elif IsCocomplexOfFinitelyPresentedObjectsRep( o!.object ) and                                  
      IsBound( HOMALG.SpectralSequenceConventionForBettiTablesOfCocomplexes ) then
        SpectralSequenceConvention := HOMALG.SpectralSequenceConventionForBettiTablesOfCocomplexes = true;
    else
        SpectralSequenceConvention := false;
    fi;
    
    if not IsBound( o!.display ) or
       o!.SpectralSequenceConvention <> SpectralSequenceConvention then
        
       o!.SpectralSequenceConvention := SpectralSequenceConvention;
       o!.display := homalgCreateDisplayStringModified( o );
        
    fi;
    
    Print( o!.display );
   fi; 
end );

###############################
#
#      simple used functions
#
################################ 

InstallGlobalFunction(GreatestCommonDivisor,
function( a, b)
while (a<>b) do
if a>b then a:=a-b;fi;
if b>a then b:=b-a;fi;
od;
return a; 
end);
######################################################################################################
InstallGlobalFunction(Factor,
function(n)
local i,s;
s:=1;
for i in [2..n] do
s:=s*i;
od;
return s;
end);

########################################################################################################
InstallGlobalFunction( NumberOfDigitsOfTheNumber,
     function(a)
return Length( String( a ) );
end);
########################################################################################################

InstallGlobalFunction(Lcmm,
     function(a,b)
      return a*b/GreatestCommonDivisor(a,b);
end);

########################################################################################################
InstallMethod(FirstNoneZeroElementInTheMatrix,
                       [IsMatrix],
      function(matrix)
      
      local i,j,b,t;
      
      b:=0;t:=0;
      for i in [1..Length(matrix)] do 
          
          for j in [1..Length(matrix[1])] do
            
             if not matrix[i][j]=0 then b:=matrix[i][j];t:=1;break;fi;
            
          od;
        
       if t=1 then break;fi;
        
       od;
return b;
end);

########################################################################################################
InstallMethod(GreatestCommonDivisorOfMatrix,
                      [IsMatrix],
            
      function(matrix)
      local i,j,b;
      b:= FirstNoneZeroElementInTheMatrix(matrix);
      for i in [1..Length(matrix)] do
       
         for j in [1..Length(matrix[1])] do 
         
           if not matrix[i][j]=0 then b:=GreatestCommonDivisor(b,matrix[i][j]);fi;
           
         od;
        
      od;
return b;
end);
         
########################################################################################################
InstallMethod(LowestCommonMultipleOfTheList,
                      [IsList],
                    
     function(list)
      
     local i,x;
      
      x:=list[1];
        
      for i in [2..Length(list)] do
          
           x:=Lcmm(x,list[i]);
      od;
          
      return x;
end);

########################################################################################################

InstallMethod( MaximumOfTheMatrix,
               [IsMatrix],
                
   function( matrix)
      local i,j, max;
      
      max:=matrix[1][1];
       
      for i in [1..Length(matrix)] do
         
         for j in [1..Length(matrix[1])] do 
          
            if max<matrix[i][j] then max:=matrix[i][j];fi;
            
         od;
        
      od;
      
    return max;
end);
##
InstallMethod( MinimumOfTheMatrix,
               [IsMatrix],
                
   function( matrix)
      local i,j, min;
      
      min:=matrix[1][1];
       
      for i in [1..Length(matrix)] do
         
         for j in [1..Length(matrix[1])] do 
          
            if min>matrix[i][j] then min:=matrix[i][j];fi;
            
         od;
        
      od;
      
    return min;
end);


########################################################################################################

# to print matrix in good shape
InstallMethod( PTM,
               [ IsMatrix ],
               
  function( matrix )
    local i,j,m,t;

     m:=1;
    for  i in [1..Length(matrix)] do
    
      for  j in [1..Length(matrix[1])] do
    
      if m<NumberOfDigitsOfTheNumber(matrix[i][j]) then m:=NumberOfDigitsOfTheNumber(matrix[i][j]);fi;
      
      od;
    od;

    Print(" _ ");

     for i in [1..Length(matrix[1])*(m+2)-2] do

         Print(" ");
     od;
    Print(" _","\n");
 
    for i in [1..Length(matrix)-1] do
        
        Print("|  ");
      
        for j in [1..Length(matrix[1])] do
             
              Print(matrix[i][j]);
                 
                  for t in [1..m+2-NumberOfDigitsOfTheNumber(matrix[i][j])] do
              
                     Print(" ");
                    
                  od;
            
         od;
       
        Print("|","\n");
     od;
      
    Print("|_ ");
     
      for j in [1..Length(matrix[1])-1] do
                  Print(matrix[Length(matrix)][j]);   

                 
                  for t in [1..m+2-NumberOfDigitsOfTheNumber(matrix[Length(matrix)][j])] do
              
                     Print(" ");
                    
                  od;

      od;
   Print(matrix[Length(matrix)][Length(matrix[1])]);
  for i in [1..m-NumberOfDigitsOfTheNumber(matrix[Length(matrix)][Length(matrix[1])])] do
  Print(" ");
  od;
  Print(" _|","\n");


 
end);

########################################################################################################
InstallMethod(IsIncreasingSequence,
              [IsList],
         function(list)
         local i,u;
         
         u:= true;
          
         for i in [1..Length(list)-1] do 
          
             if list[i]>list[i+1] then u:=false;fi;
         
         od;
        
         return u;
end);
 


####################################
#
#   Operations
#
####################################

# this program return the sum of two Betti diagrams 
InstallMethod(\+,
              [IsVirtualBettiTable,IsVirtualBettiTable],
function(A0,B0)
local A,B, A1,B1,C1,col_range1, row_range1,col_range2,row_range2,col_range3, row_range3, min, max;

A := A0!.Object;
B := B0!.Object;

A1:= A!.matrix; B1:= B!.matrix;

row_range1:= A!.row_range;
row_range2:= B!.row_range;

col_range1:= A!.column_range;
col_range2:= B!.column_range;

#this code is for virtual cohomology tables ...

if A0!.IsCohomology= true and row_range1=row_range2 and col_range1=col_range2 then 

              return ConvertToVirtualBettiTable( HomalgBettiTable( A1+B1, row_range1 , col_range1 , HomalgCocomplex( HOMALG_MATRICES.ZZ ), false ), false, true);
              
fi;


#this code is for virtual Betti tables ...

if A0!.IsBetti= true then 

   if Length(col_range1)> Length(col_range2)  then B1:= AddEmptyColumnsToTheMatrix(B1, Length(col_range1)- Length(col_range2) );fi;
   if Length(col_range1)< Length(col_range2)  then A1:= AddEmptyColumnsToTheMatrix(A1, Length(col_range2)- Length(col_range1) );fi;

   min:= Minimum(row_range1[1],row_range2[1]);
   max:= Maximum(row_range1[Length(row_range1)],row_range2[Length(row_range2)]);

   A1:= AddEmptyLinesToTheMatrix(A1,row_range1[1]-min,max- row_range1[Length(row_range1)]);
   B1:= AddEmptyLinesToTheMatrix(B1,row_range2[1]-min,max- row_range2[Length(row_range2)]);
   C1:=A1+B1;

   row_range3:= [ min..max ];

   if Length(col_range1)= Length(col_range2)  then col_range3:= col_range1 ;fi;
   if Length(col_range1)> Length(col_range2)  then col_range3:= col_range1 ;fi;
   if Length(col_range1)< Length(col_range2)  then col_range3:= col_range2 ;fi;

return ConvertToVirtualBettiTable( HomalgBettiTable(C1, row_range3, col_range3, rec( string := "" )), true, false);

fi;
end );

# this program return the multiplication of a betti diagram with a rational numebr 
InstallMethod(\*, 
                [IsRat, IsVirtualBettiTable],

function(a, v_betti)
local betti;

betti:=v_betti!.Object;

if v_betti!.IsBetti= true then  
 
           return ConvertToVirtualBettiTable( HomalgBettiTable(a*betti!.matrix ,betti!.row_range, betti!.column_range, rec(string := "")), true, false);
           
fi;

if v_betti!.IsCohomology= true then 

           return ConvertToVirtualBettiTable( HomalgBettiTable( a*betti!.matrix, betti!.row_range, betti!.column_range, HomalgCocomplex( HOMALG_MATRICES.ZZ ), false ), false, true);
          
fi;

end);


# this program return the multiplication of a betti diagram with a rational numebr 
InstallMethod(\*, 
                [IsVirtualBettiTable, IsRat],

function(v_betti, a)
local betti;

betti:=v_betti!.Object;

if v_betti!.IsBetti= true then  
 
           return ConvertToVirtualBettiTable( HomalgBettiTable(a*betti!.matrix ,betti!.row_range, betti!.column_range, rec(string := "")), true, false);
           
fi;

if v_betti!.IsCohomology= true then 

           return ConvertToVirtualBettiTable( HomalgBettiTable( a*betti!.matrix, betti!.row_range, betti!.column_range, HomalgCocomplex( HOMALG_MATRICES.ZZ ), false ), false, true);
          
fi;

end);

# this program return the multiplication of an equation with a rational numebr.

InstallMethod(\*,
                [IsUpperEquationOfFacet, IsRat],

function(equation, b)
return UpperEquationOfFacet(b*equation!.MatrixWithCoefficientsOfTheUpperEquation, equation!.Grades, equation!.IsForRootSequence, equation!.IsForDegreeSequence);
end );
            
InstallMethod(\*,
                [IsRat, IsUpperEquationOfFacet],

function(b,equation)
return UpperEquationOfFacet(b*equation!.MatrixWithCoefficientsOfTheUpperEquation, equation!.Grades, equation!.IsForRootSequence, equation!.IsForDegreeSequence);
end );

InstallMethod(\*,
                [IsLowerEquationOfFacet, IsRat],

function(equation, b)
return LowerEquationOfFacet(b*equation!.MatrixWithCoefficientsOfTheLowerEquation, equation!.Grades);
end );
            
InstallMethod(\*,
                [IsRat, IsLowerEquationOfFacet],

function(b,equation)
return LowerEquationOfFacet(b*equation!.MatrixWithCoefficientsOfTheLowerEquation, equation!.Grades);
end );  

	