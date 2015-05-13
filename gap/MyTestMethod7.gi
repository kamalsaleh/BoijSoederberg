#################################
##
## Representations
##
#################################

DeclareRepresentation( "IsPosetOfRootSequencesRep",
                       IsPosetOfRootSequences and IsAttributeStoringRep,
                       [ ] );

DeclareRepresentation( "IsRootSequenceRep",
                       IsRootSequence and IsAttributeStoringRep,
                       [ "StrictlyDecreasingIntegerSequence" ] );

DeclareRepresentation( "IsIntervalOfRootSequencesRep",
                       IsIntervalOfRootSequences and IsAttributeStoringRep,
                       [ ] );

DeclareRepresentation( "IsMorphismOfIntervalsOfRootSequencesRep",
                       IsMorphismOfIntervalsOfRootSequences and IsStaticMorphismOfFinitelyGeneratedObjectsRep,
                       [ ] );

DeclareRepresentation( "IsVirtualCohomologyTableRep",
                       IsVirtualCohomologyTable and IsAttributeStoringRep,
                       [ ] );
                       
DeclareRepresentation( "IsCombinatorialVirtualCohomologyTableRep",
                       IsVirtualCohomologyTableRep,
                       [ "LinearCombinationOfRootSequences" ] );

DeclareRepresentation( "IsVirtualHilbertPolynomialRep",
                       IsVirtualHilbertPolynomial and IsAttributeStoringRep,
                       [ "ListOfCoefficients" ] );

DeclareRepresentation( "IsVectorSpaceWithIntegralStructureRep",
                       IsVectorSpaceWithIntegralStructure and IsAttributeStoringRep,
                       [ "LatticePosition" ] );
                       
DeclareRepresentation( "IsMorphismOfVectorSpacesWithIntegralStructureRep",
                       IsMorphismOfVectorSpacesWithIntegralStructure and IsStaticMorphismOfFinitelyGeneratedObjectsRep,
                       [ "LatticePosition" ] );

DeclareRepresentation( "IsHomalgCategoryRep",
                       IsHomalgCategory and IsAttributeStoringRep ,
                       ["thisIsTest" ] );

##################################
##
## Family and Type
##
##################################

BindGlobal( "TheBoijSoederbergFamily",
  NewFamily( "TheBoijSoederbergFamily", IsObject ) );

BindGlobal( "TheTypePosetOfRootSequences",
  NewType( TheBoijSoederbergFamily, 
           IsPosetOfRootSequencesRep ) );

BindGlobal( "TheTypeRootSequence",
  NewType( TheBoijSoederbergFamily, 
           IsRootSequenceRep ) );

BindGlobal( "TheTypeIntervalOfRootSequences",
  NewType( TheBoijSoederbergFamily,
           IsIntervalOfRootSequencesRep ) );

BindGlobal( "TheTypeMorphismOfIntervalsOfRootSequences",
  NewType( TheBoijSoederbergFamily,
           IsMorphismOfIntervalsOfRootSequencesRep ) );

BindGlobal( "TheTypeCombinatorialVirtualCohomologyTable",
  NewType( TheBoijSoederbergFamily,
           IsCombinatorialVirtualCohomologyTableRep ) );

BindGlobal( "TheTypeVirtualHilbertPolynomial",
  NewType( TheBoijSoederbergFamily,
           IsVirtualHilbertPolynomialRep ) );

BindGlobal( "TheTypeVectorSpaceWithIntegralStructure",
  NewType( TheBoijSoederbergFamily,
           IsVectorSpaceWithIntegralStructureRep ) );

BindGlobal( "TheTypeMorphismOfVectorSpacesWithIntegralStructure",
  NewType( TheBoijSoederbergFamily,
           IsMorphismOfVectorSpacesWithIntegralStructureRep ) );

BindGlobal( "TheTypeCategory",
            NewType( TheBoijSoederbergFamily, IsHomalgCategoryRep ) );

#################################
##
## Mathematical Categories and Functors
##
#################################

## Mathematical Categories
##
InstallValue( BOIJ_SOEDERBERG,
  rec(
        PosetOfRootSequences := WeakPointerObj( [ ] ),
     
        CategoryOfIntervalsOfRootSequences :=
          Objectify(  TheTypeCategory,
            rec(
                  description := "category of intervals of root sequences",
                  short_description :=  "intervals",
                  MorphismConstructor := MorphismOfIntervalsOfRootSequences,
                  containers := rec( )
               )
            ),

        CategoryOfVectorSpacesOfVirtualHilbertPolynomials :=
          Objectify( TheTypeCategory,
            rec(
                  description := "category of vector spaces of virtual Hilbert polynomials",
                  short_description := "virtual Hilbert polynomials",
                  MorphismConstructor := ReturnTrue,
                  containers := rec( ),
                  FullVectorSpaceOfVirtualHilbertPolynomials := WeakPointerObj( [ ] ),
                  QQ := HomalgFieldOfRationals( )
               )
            )
            
     )
     
);

BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables :=
  Objectify( TheTypeCategory,
    rec(
          description := "category of vector spaces of virtual cohomology tables",
          short_description := "virtual cohomology tables",
          MorphismConstructor := ReturnTrue,
          containers := rec( ),
          QQ := BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ,
          display_interval := [ -5 .. 5 ]
       )
       
);

## this method is mandatory:
##
## this is done by kamal deleting the following code
#InstallMethod( HomalgCategory,
#               "for intervals of root sequences",
#               [ IsIntervalOfRootSequencesRep ],
#  function( interval_of_root_sequences )

#    return BOIJ_SOEDERBERG.CategoryOfIntervalsOfRootSequences;

#end );

##
#InstallMethod( HomalgCategory,
#               "for vector spaces of virtual Hilbert polynomials",
#               [ IsVectorSpaceWithIntegralStructureRep and IsEmbedded and IsConcrete ],
#  function( vector_space_of_virtual_hilbert_polynomials )

#    return BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials;

# end );

##
#InstallMethod( HomalgCategory,
    #           "for vector spaces of virtual cohomology tables",
   #            [ IsVectorSpaceWithIntegralStructureRep and IsConcrete ],
  #function( vector_space_of_virtual_cohomology_tables )

 #   return BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables;

#end );

## VectorSpaceOfVirtualHilbertPolynomials
##
InstallValue( functor_VectorSpaceOfVirtualHilbertPolynomials,
  CreateHomalgFunctor(
    [ "name", "VectorSpaceOfVirtualHilbertPolynomials" ],
    [ "category", BOIJ_SOEDERBERG.CategoryOfIntervalsOfRootSequences ],
    [ "operation", "VectorSpaceOfVirtualHilbertPolynomials" ],
    [ "number_of_arguments", 1 ],
    [ "1", [ [ "covariant" ], [ IsIntervalOfRootSequences, IsMorphismOfIntervalsOfRootSequences ] ] ],
    [ "OnObjects", _Functor_VectorSpaceOfVirtualHilbertPolynomials_OnIntervalOfRootSequences ],
    [ "OnMorphisms", _Functor_VectorSpaceOfVirtualHilbertPolynomials_OnMorphismOfIntervalsOfRootSequences ] )
);

functor_VectorSpaceOfVirtualHilbertPolynomials!.ContainerForWeakPointersOnComputedBasicObjects := true;

functor_VectorSpaceOfVirtualHilbertPolynomials!.ContainerForWeakPointersOnComputedBasicMorphisms := true;

InstallFunctor( functor_VectorSpaceOfVirtualHilbertPolynomials );

##
InstallGlobalFunction( _Functor_VectorSpaceOfVirtualHilbertPolynomials_OnIntervalOfRootSequences,
  function( interval_of_root_sequences )
    local length_of_root_sequences;

      length_of_root_sequences := LengthOfRootSequences( UnderlyingPosetOfRootSequences( interval_of_root_sequences ) );

      return SubVectorSpaceOfHilbertPolynomials(
               List( TopMaximalHilbertChain( interval_of_root_sequences ), HilbertPolynomial ), length_of_root_sequences
             );

end );

##
InstallGlobalFunction( _Functor_VectorSpaceOfVirtualHilbertPolynomials_OnMorphismOfIntervalsOfRootSequences,
  function( F_source, F_range, arg_before_pos, morphism, arg_behind_pos )
    local embedding_matrix_of_source, embedding_matrix_of_target, homalg_map;

    embedding_matrix_of_source := MatrixOfMap( UnderlyingMorphism( EmbeddingIntoSuperVectorSpace( F_source ) ) );

    embedding_matrix_of_target := MatrixOfMap( UnderlyingMorphism( EmbeddingIntoSuperVectorSpace( F_range ) ) );

    homalg_map := HomalgMap( embedding_matrix_of_source * RightInverse( embedding_matrix_of_target ),
                             UnderlyingVectorSpace( F_source ),
                             UnderlyingVectorSpace( F_range ),
                             BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ
                           );
                             
    return MorphismOfVectorSpacesWithIntegralStructure( F_source, F_range, homalg_map );

end );

## VectorSpaceOfVirtualCohomologyTables
##
InstallValue( functor_VectorSpaceOfVirtualCohomologyTables,
  CreateHomalgFunctor(
    [ "name", "VectorSpaceOfVirtualCohomologyTables" ],
    [ "category", BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables ],
    [ "operation", "VectorSpaceOfVirtualCohomologyTables" ],
    [ "number_of_arguments", 1 ],
    [ "1", [ [ "covariant" ], [ IsIntervalOfRootSequences, IsMorphismOfIntervalsOfRootSequences ] ] ],
    [ "OnObjects", _Functor_VectorSpaceOfVirtualCohomologyTables_OnIntervalOfRootSequences ],
    [ "OnMorphisms", _Functor_VectorSpaceOfVirtualCohomologyTables_OnMorphismOfIntervalsOfRootSequences ] )
);

functor_VectorSpaceOfVirtualCohomologyTables!.ContainerForWeakPointersOnComputedBasicObjects := true;

functor_VectorSpaceOfVirtualCohomologyTables!.ContainerForWeakPointersOnComputedBasicMorphisms := true;

InstallFunctor( functor_VectorSpaceOfVirtualCohomologyTables );

##
InstallGlobalFunction( _Functor_VectorSpaceOfVirtualCohomologyTables_OnElementaryIntervals,
  function( root_sequence, interval_of_root_sequences )
    local morphism, row, coefficients, virtual_cohomology_table, kernel_entry;
  
    morphism :=
          VectorSpaceOfVirtualHilbertPolynomials(
            MorphismOfIntervalsOfRootSequences( IntervalOfRootSequences( root_sequence ), interval_of_root_sequences )
          );

    if not IsIdenticalObj( Source( UnderlyingMorphism( morphism ) ), Range( UnderlyingMorphism( morphism ) ) ) then
          
      SetPositionOfTheDefaultPresentation( Source( UnderlyingMorphism( morphism ) ), PositionOfConcretePresentation( Source( morphism ) ) );

      SetPositionOfTheDefaultPresentation( Range( UnderlyingMorphism( morphism ) ), Range( morphism )!.LatticePosition );

      row := EntriesOfHomalgMatrix( MatrixOfMap( UnderlyingMorphism( morphism ) ) );

      SetPositionOfTheDefaultPresentation( Range( UnderlyingMorphism( morphism ) ), PositionOfConcretePresentation( Range( morphism ) ) );

      coefficients := EntriesOfHomalgMatrix( MatrixOfMap( UnderlyingMorphism( morphism ) ) );

    else # an ugly case

      row :=
        EntriesOfHomalgMatrix(
          UnderlyingVectorSpace( Source( morphism ) )!.TransitionMatrices.(
            Concatenation( "[ ", String( PositionOfConcretePresentation( Source( morphism ) ) ), ", ",
            String( Source( morphism )!.LatticePosition ), " ]")
          )
        );

      coefficients := [ 1 ];

    fi;

    virtual_cohomology_table :=
      CohomologyTable( root_sequence ) + ( - 1 ) *
      VirtualCohomologyTable(
        List( [ 1 .. Length( coefficients ) ], i -> [ coefficients[ i ], TopMaximalHilbertChain( interval_of_root_sequences )[ i ] ] )
      );

    for kernel_entry in KernelEntries( interval_of_root_sequences ) do

      Add( row, ( -1 )^( kernel_entry[ 1 ] ) * virtual_cohomology_table[ kernel_entry ] );

    od;

    return row;

end );

##
InstallGlobalFunction( _Functor_VectorSpaceOfVirtualCohomologyTables_OnIntervalOfRootSequences,
  function( interval_of_root_sequences )
    local length_of_root_sequences, concrete_basis, lattice_basis, dimension, matrix, root_sequence, morphism, row,
          coefficients, virtual_cohomology_table, kernel_entry, homalg_map, vector_space_of_virtual_cohomology_tables;

      length_of_root_sequences := LengthOfRootSequences( UnderlyingPosetOfRootSequences( interval_of_root_sequences ) );

      concrete_basis := TopMaximalChain( interval_of_root_sequences );

      lattice_basis := Concatenation(
                         TopMaximalHilbertChain( interval_of_root_sequences ),
                         KernelEntries( interval_of_root_sequences )
                       );

      dimension := Length( lattice_basis );

      if not dimension = Length( concrete_basis ) then

        Error( "The concrete basis and the lattice basis do not have the same number of vectors. This is either a bug or a mathematical error\n" );
      
      fi;

      matrix := [ ];

      for root_sequence in concrete_basis do

        row := _Functor_VectorSpaceOfVirtualCohomologyTables_OnElementaryIntervals( root_sequence, interval_of_root_sequences );

        Add( matrix, row );

      od;

      homalg_map := HomalgMap(
        matrix,
        HomalgFreeLeftModule( dimension, BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.QQ ),
        HomalgFreeLeftModule( dimension, BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.QQ ),
        BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.QQ
      );

      if not IsIsomorphism( homalg_map ) then

        Error( "The base change matrix concrete -> lattice is not an isomorphism. This is either a bug or a mathematical error\n" );
      
      fi;
      
      vector_space_of_virtual_cohomology_tables := VectorSpaceWithIntegralStructure( PreInverse( homalg_map ) );
      
      SetIsConcrete( vector_space_of_virtual_cohomology_tables, true );

      SetPositionOfConcretePresentation( vector_space_of_virtual_cohomology_tables, 1 );

      return vector_space_of_virtual_cohomology_tables;

end );

##
InstallGlobalFunction( _Functor_VectorSpaceOfVirtualCohomologyTables_OnMorphismOfIntervalsOfRootSequences,
  function( F_source, F_range, arg_before_pos, morphism, arg_behind_pos )
    local interval_of_root_sequences_of_source, interval_of_root_sequences_of_range,
    concrete_basis_of_source, matrix, root_sequence, row, homalg_map;

    if IsIdenticalObj( F_source, F_range ) then

      return MorphismOfVectorSpacesWithIntegralStructure( F_source, F_range, HomalgIdentityMap( UnderlyingVectorSpace( F_source ) ) );
    
    fi;

    interval_of_root_sequences_of_source := F_source!.Genesis[ 1 ][ 1 ].arguments_of_functor[ 1 ];

    interval_of_root_sequences_of_range := F_range!.Genesis[ 1 ][ 1 ].arguments_of_functor[ 1 ];

    concrete_basis_of_source := TopMaximalChain( interval_of_root_sequences_of_source );

    matrix := [ ];

    for root_sequence in concrete_basis_of_source do

      row := _Functor_VectorSpaceOfVirtualCohomologyTables_OnElementaryIntervals( root_sequence, interval_of_root_sequences_of_range );

      Add( matrix, row );

    od;

    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( F_source ), PositionOfConcretePresentation( F_source ) );

    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( F_range ), F_range!.LatticePosition );
    
    homalg_map := HomalgMap(
        matrix,
        UnderlyingVectorSpace( F_source ),
        UnderlyingVectorSpace( F_range ),
        BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.QQ
      );
    
    return MorphismOfVectorSpacesWithIntegralStructure( F_source, F_range, homalg_map );

end );

## FullVectorSpaceOfVirtualHilbertPolynomials
##
InstallMethod( FullVectorSpaceOfVirtualHilbertPolynomials,
               "for an integer",
               [ IsInt ],

  function( degree )
    local homalg_map, full_vector_space_of_virtual_hilbert_polynomials;
  
    if not IsBound( BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.FullVectorSpaceOfVirtualHilbertPolynomials[ degree ] ) then    

      homalg_map := HomalgMap(
                               List(
#                                  List( [ 0 .. degree ], i -> [ 1 + i .. degree + i ] ),
                                 List( [ 0 .. degree ], i -> [ -degree + i .. -1 + i ] ),
                                 L -> RowOfCoefficients( HilbertPolynomial( RootSequence( L ) ), degree )
                               ),
                               ( degree + 1 ) * BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ,
                               ( degree + 1 ) * BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ,
                               BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ
                             );

      if not IsIsomorphism( homalg_map ) then

        Error( "structure morphism of the vector space of Hilbert polynomials is not an isomorphism. This is either a bug or a mathematical error\n" );

      fi;

      full_vector_space_of_virtual_hilbert_polynomials := VectorSpaceWithIntegralStructure( homalg_map );
      
      BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.FullVectorSpaceOfVirtualHilbertPolynomials[ degree ] :=
        full_vector_space_of_virtual_hilbert_polynomials;

    fi;

    return BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.FullVectorSpaceOfVirtualHilbertPolynomials[ degree ];
  
end );
 
##################################
##
## Properties
##
##################################

##
InstallMethod( IsIntegral,
               "for a virtual Hilbert polynomial",
               [ IsVirtualHilbertPolynomialRep ],

  function( virtual_hilbert_polynomial )
    local super_space, homalg_map, list_of_coefficients;

    super_space := FullVectorSpaceOfVirtualHilbertPolynomials( Length( virtual_hilbert_polynomial!.ListOfCoefficients ) - 1 );

    # the standard basis 1, x, x^2, ...
    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( super_space ), 1 );
    
    homalg_map :=
      HomalgMap(
        [ virtual_hilbert_polynomial!.ListOfCoefficients ],
        HomalgFreeLeftModule( 1, BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ ),
        UnderlyingVectorSpace( super_space ),
        BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ
      );
    
    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( super_space ), super_space!.LatticePosition );
    
    list_of_coefficients := EntriesOfHomalgMatrix( MatrixOfMap( homalg_map ) );

    return not false in List( list_of_coefficients, IsInt  );
      
end );

##
InstallMethod( IsIntegral,
               "for a virtual cohomology table",
               [ IsVirtualCohomologyTableRep ],

  function( virtual_cohomology_table )
    local interval_of_minimal_ambient_space, ambient_space, morphism, row,
    pair, homalg_map, list_of_coefficients;
  
    interval_of_minimal_ambient_space := IntervalOfMinimalAmbientSpace( virtual_cohomology_table );

    ambient_space := VectorSpaceOfVirtualCohomologyTables( interval_of_minimal_ambient_space );

    row := [ ];
    
    for pair in virtual_cohomology_table!.LinearCombinationOfRootSequences do

      morphism :=
        VectorSpaceOfVirtualCohomologyTables(
          MorphismOfIntervalsOfRootSequences( IntervalOfRootSequences( pair[ 2 ] ), interval_of_minimal_ambient_space )
        );

      SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( ambient_space ), PositionOfConcretePresentation( ambient_space ) );

      SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( Source( morphism ) ), PositionOfConcretePresentation( Source( morphism ) ) );

      row := pair[ 1 ] * EntriesOfHomalgMatrix( MatrixOfMap( UnderlyingMorphism( morphism ) ) ) + row;

    od;

    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( ambient_space ), PositionOfConcretePresentation( ambient_space ) );
    
    homalg_map :=
      HomalgMap( [ row ],
      HomalgFreeLeftModule( 1, BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.QQ ),
      UnderlyingVectorSpace( ambient_space ),
      BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.QQ
      );

    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( ambient_space ), ambient_space!.LatticePosition );

    list_of_coefficients := EntriesOfHomalgMatrix( MatrixOfMap( homalg_map ) );
    
    return not false in List( list_of_coefficients, IsInt  );

end );

##################################
##
## Attributes
##
##################################

## IsRootSequence
##
InstallMethod( HilbertPolynomial,
               "for a root sequence",
               [ IsRootSequenceRep ],
               
  function( root_sequence )
    local t, hilbert_polynomial, length_of_root_sequences;
    
    t := VariableForHilbertPolynomial( );

    length_of_root_sequences := LengthOfRootSequences( UnderlyingPosetOfRootSequences( root_sequence ) );
    
    hilbert_polynomial := ( 1 / Factorial( length_of_root_sequences ) ) * Product( List( root_sequence!.StrictlyDecreasingIntegerSequence ), omega -> t - omega );
    
    ## it is ugly that we need this
    SetIndeterminateOfUnivariateRationalFunction( hilbert_polynomial, t );

    ## checking this property sets it
    Assert( 0, IsUnivariatePolynomial( hilbert_polynomial ) );
    
    return hilbert_polynomial;
    
end );

##
InstallMethod( CohomologyTable,
               "for a root sequence",
               [ IsRootSequenceRep ],

  function( root_sequence )

    return VirtualCohomologyTable( [ [ 1, root_sequence ] ] );
  
end );

## IsIntervalOfRootSequences
##
InstallMethod( UnderlyingSet,
               "for an interval of root sequences",
               [ IsIntervalOfRootSequencesRep ],
               
  function( interval_of_root_sequences )
    local left_boundary, right_boundary, AllListsWithGivenRightSide, n, help_list, result_list;

    left_boundary := Reversed( LeftBoundary( interval_of_root_sequences )!.StrictlyDecreasingIntegerSequence );

    right_boundary := Reversed( RightBoundary( interval_of_root_sequences )!.StrictlyDecreasingIntegerSequence );

    AllListsWithGivenRightSide := function( list, a )
      local n, j, l, help_list;

      n := Length( list );
      
      if n = 1 then

        return List( [ list[ n ] .. Minimum( a - 1, right_boundary[ n ] ) ], j -> [ j ] );

      fi;

      help_list := [ ];

      for j in [ list[ n ] .. Minimum( a - 1, right_boundary[ n ] ) ] do

        for l in AllListsWithGivenRightSide( list{ [ 1 .. n - 1 ] }, j ) do

          Add( l, j );

          Add( help_list, l );

        od;
        
      od;

      return help_list;
      
    end;

    n := Length( right_boundary );

    help_list := AllListsWithGivenRightSide( left_boundary, right_boundary[ Length( right_boundary ) ] + 1 );

    help_list := help_list{ [ 2 .. Length( help_list ) - 1 ] };

    result_list := [ LeftBoundary( interval_of_root_sequences ) ];

    Append( result_list, List( help_list, l -> RootSequence( l, UnderlyingPosetOfRootSequences( interval_of_root_sequences ) ) ) );

    Add( result_list, RightBoundary( interval_of_root_sequences ) );

    return Set( result_list );

end );

##
InstallMethod( TopMaximalChain,
               "for an interval of root sequences",
               [ IsIntervalOfRootSequencesRep ],
  function( interval_of_root_sequences )
    local underlying_poset_of_root_sequences, length_of_root_sequences, top_maximal_chain, i, j, chain_element;

    underlying_poset_of_root_sequences := UnderlyingPosetOfRootSequences( interval_of_root_sequences );

    length_of_root_sequences := LengthOfRootSequences( underlying_poset_of_root_sequences );

    top_maximal_chain := [ LeftBoundary( interval_of_root_sequences ) ];

    chain_element := ShallowCopy( LeftBoundary( interval_of_root_sequences )!.StrictlyDecreasingIntegerSequence );
    
    for i in [ 1 .. length_of_root_sequences ] do
    
      for j in [ LeftBoundary( interval_of_root_sequences )[ i ] + 1 .. RightBoundary( interval_of_root_sequences )[ i ] ] do

        chain_element[ i ] := j;

        Add( top_maximal_chain, RootSequence( chain_element, underlying_poset_of_root_sequences ) );
      
      od;
    
    od;

    top_maximal_chain := top_maximal_chain{ [ 1 .. Length( top_maximal_chain ) - 1 ] };

    Add( top_maximal_chain, RightBoundary( interval_of_root_sequences ) );

    return top_maximal_chain;

end );

##
InstallMethod( TopMaximalHilbertChain,
               "for an interval of root sequences",
               [ IsIntervalOfRootSequencesRep ],
  function( interval_of_root_sequences )
    local underlying_poset_of_root_sequences, length_of_root_sequences, new_left_boundary, new_interval;

    underlying_poset_of_root_sequences := UnderlyingPosetOfRootSequences( interval_of_root_sequences );

    length_of_root_sequences := LengthOfRootSequences( underlying_poset_of_root_sequences );

    new_left_boundary := List( [ 1 .. length_of_root_sequences ],
                               function( i )

                                 if LeftBoundary( interval_of_root_sequences )[ i ] = RightBoundary( interval_of_root_sequences )[ i ]  then

                                   return RightBoundary( interval_of_root_sequences )[ i ];

                                 else

                                   return RightBoundary( interval_of_root_sequences )[ i ] - 1;

                                 fi;

                               end );

    new_left_boundary := RootSequence( new_left_boundary, underlying_poset_of_root_sequences );
                                 
    new_interval := IntervalOfRootSequences( new_left_boundary, RightBoundary( interval_of_root_sequences ) );

    return TopMaximalChain( new_interval );
    
end );

##
InstallMethod( KernelEntries,
               "for an interval of root sequences",
               [ IsIntervalOfRootSequences ],

  function( interval_of_root_sequences )
    local length_of_root_sequences, entry, intervals;

    length_of_root_sequences := LengthOfRootSequences( UnderlyingPosetOfRootSequences( interval_of_root_sequences ) );

    intervals := [ ];
    
    for entry in [ 1 .. length_of_root_sequences ] do

      Append( intervals,
        List( [ LeftBoundary( interval_of_root_sequences )[ entry ] + 1 .. RightBoundary( interval_of_root_sequences )[ entry ] - 1 ], i -> [ entry, i ] ) );
    
    od;

    return intervals;

end );

## IsVirtualCohomologyTable
InstallMethod( IntervalOfMinimalAmbientSpace,
               "for a virtual cohomology table",
               [ IsVirtualCohomologyTableRep ],

  function( virtual_cohomology_table )               
    local necessary_root_sequences;
  
    necessary_root_sequences :=
      List( virtual_cohomology_table!.LinearCombinationOfRootSequences, pair -> pair[ 2 ] );

    return IntervalOfRootSequences( Infimum( necessary_root_sequences ), Supremum( necessary_root_sequences ) );
               
end );

##
InstallMethod( HilbertPolynomial,
               "for a virtual cohomology table",
               [ IsVirtualCohomologyTableRep ],

  function( virtual_cohomology_table )

    return Sum( List( virtual_cohomology_table!.LinearCombinationOfRootSequences, l -> l[ 1 ] * HilbertPolynomial( l[ 2 ] ) ) );

end );

##
InstallMethod( BettiTable,
               "for a virutal character table",
               [ IsVirtualCohomologyTableRep ],

  function( virtual_cohomology_table )
    local n, betti_table;

    n := LengthOfRootSequences( UnderlyingPosetOfRootSequences( virtual_cohomology_table ) );

    betti_table := HomalgBettiTable(
      Reversed(
        List( [ 0 .. n ], i -> List( BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.display_interval,
        D -> virtual_cohomology_table[ [ i, D ] ] ) )
      ), [ 0 .. n ], BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.display_interval, virtual_cohomology_table
    );

    betti_table!.reverse := true;

    return betti_table;

end );

## IsVectorSpaceWithIntegralStructure
##
InstallMethod( Dimension,
               "for a vector space with integral structure",
               [ IsVectorSpaceWithIntegralStructureRep ],

  function( vector_space_with_integral_structure )

    return Rank( UnderlyingVectorSpace( vector_space_with_integral_structure ) );
  
end );

##IsVirtualHilbertPolynomial
##
InstallMethod( EmbeddingIntoSuperVectorSpace,
               "for a virtual Hilbert polynomial",
               [ IsVirtualHilbertPolynomialRep ],

  function( virtual_hilbert_polynomial )
    local length, full_vector_space_of_virtual_hilbert_polynomials, homalg_map;

    length := Length( virtual_hilbert_polynomial!.ListOfCoefficients );
    
    full_vector_space_of_virtual_hilbert_polynomials := FullVectorSpaceOfVirtualHilbertPolynomials( length - 1 );

    # the standard basis 1, x, ..., x^length_of_root_sequences
    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( full_vector_space_of_virtual_hilbert_polynomials ), 1 );

    homalg_map := HomalgMap( [ virtual_hilbert_polynomial!.ListOfCoefficients ],
                             HomalgFreeLeftModule( 1, BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ ),
                             UnderlyingVectorSpace( full_vector_space_of_virtual_hilbert_polynomials ),
                             BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ );

    return homalg_map;
  
end );

##
InstallMethod( K0ElementOfStableModuleCategory,
               "for a virtual Hilbert polynomial",
               [ IsVirtualHilbertPolynomialRep ],

  function( virtual_hilbert_polynomial )
    local length, embedding_into_super_vector_space, full_vector_space_of_virtual_hilbert_polynomials, coefficients;

    length := Length( virtual_hilbert_polynomial!.ListOfCoefficients );

    embedding_into_super_vector_space := EmbeddingIntoSuperVectorSpace( virtual_hilbert_polynomial );

    full_vector_space_of_virtual_hilbert_polynomials := FullVectorSpaceOfVirtualHilbertPolynomials( length - 1 );

    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( full_vector_space_of_virtual_hilbert_polynomials ), full_vector_space_of_virtual_hilbert_polynomials!.LatticePosition );

    coefficients := EntriesOfHomalgMatrix( MatrixOfMap( embedding_into_super_vector_space ) );
    
    return ElementOfGradedRelativeRing( List( [ 1 .. length ], i -> [ ( - 1 )^( i - 1 ) * coefficients[ i ], i - 1 ] ), length );
    
end );

##
InstallMethod( TwistedChernPolynomial,
               "for a virtual Hilbert polynomial",
               [ IsVirtualHilbertPolynomial ],

  function( virtual_hilbert_polynomial )
    local k0_element, chern_polynomial, chern_class_1, rank, twist;

    k0_element := K0ElementOfStableModuleCategory( virtual_hilbert_polynomial );
    
    chern_polynomial := ChernPolynomial( k0_element );

    chern_class_1 := Coefficients( chern_polynomial )[1];

    rank := Rank( chern_polynomial );

    twist := QuoInt( chern_class_1, rank );

    if ( chern_class_1 > 0 ) and ( RemInt( chern_class_1, rank ) <> 0 ) then

      twist := twist + 1;

    fi;
    
    return [ ChernPolynomial( ( - 1 )^( twist ) * VerticalShift( k0_element, twist ) ), twist ];
  
end );

##################################
##
## Operations
##
##################################

#TODO: Check convention for root_sequence2
##
InstallMethod( Infimum,
               "for a pair of root sequences",
               [ IsRootSequenceRep, IsRootSequenceRep ],

  function( root_sequence1, root_sequence2 )
    local length_of_root_sequences, infimum, i;

    if not IsIdenticalObj( UnderlyingPosetOfRootSequences( root_sequence1 ), UnderlyingPosetOfRootSequences( root_sequence2 ) ) then

      Error( "the root sequences are not defined over the identical poset of root sequences\n" );
    
    fi;
    
    length_of_root_sequences := LengthOfRootSequences( UnderlyingPosetOfRootSequences( root_sequence1 ) );
    
    infimum := [ ];

    for i in [ 1 .. length_of_root_sequences ] do
  
      Add( infimum, Minimum( root_sequence1[ i ], root_sequence2[ i ] ) );
    
    od;

    return RootSequence( infimum, UnderlyingPosetOfRootSequences( root_sequence1 ) );

end );

##
InstallMethod( Infimum,
               "for a list",
               [ IsList ],

  function( list )
    local infimum, i;
  
    if false in List( list, l -> IsRootSequenceRep( l ) ) then

      Error( "the list must contain only root sequences" );

    fi;

    infimum := list[ 1 ];

    for i in [ 2 .. Length( list ) ] do

      infimum := Infimum( infimum, list[ i ] );
    
    od;

    return infimum;

end );

##
InstallMethod( Supremum,
               "for a pair of root sequences",
               [ IsRootSequenceRep, IsRootSequenceRep ],

  function( root_sequence1, root_sequence2 )
    local length_of_root_sequences, supremum, i;

    if not IsIdenticalObj( UnderlyingPosetOfRootSequences( root_sequence1 ), UnderlyingPosetOfRootSequences( root_sequence2 ) ) then

      Error( "the root sequences are not defined over the identical poset of root sequences\n" );

    fi;

    length_of_root_sequences := LengthOfRootSequences( UnderlyingPosetOfRootSequences( root_sequence1 ) );

    supremum := [ ];

    for i in [ 1 .. length_of_root_sequences ] do

      Add( supremum, Maximum( root_sequence1[ i ], root_sequence2[ i ] ) );

    od;

    return RootSequence( supremum, UnderlyingPosetOfRootSequences( root_sequence1 ) );

end );

##
InstallMethod( Supremum,
               "for a list",
               [ IsList ],

  function( list )
    local supremum, i;

    if false in List( list, l -> IsRootSequenceRep( l ) ) then

      Error( "the list must contain only root sequences" );

    fi;

    supremum := list[ 1 ];

    for i in [ 2 .. Length( list ) ] do

      supremum := Supremum( supremum, list[ i ] );

    od;

    return supremum;

end );            

##
InstallMethod( Value,
               "for a virtual Hilbert polynomial and a rational",
               [ IsVirtualHilbertPolynomial, IsRat ],

  function( virtual_hilbert_polynomial, rational )

    return Value( UnderlyingPolynomial( virtual_hilbert_polynomial ), rational );
  
end );

##
InstallMethod( \+,
               "for a pair of virtual Hilbert polynomials",
               [ IsVirtualHilbertPolynomialRep, IsVirtualHilbertPolynomialRep ],

  function( virtual_hilbert_polynomial1, virtual_hilbert_polynomial2 )

    return VirtualHilbertPolynomial( UnderlyingPolynomial( virtual_hilbert_polynomial1 ) + UnderlyingPolynomial( virtual_hilbert_polynomial2 ) );
  
end );

##
InstallMethod( \*,
               "for a virtual Hilbert polynomial and a rational number",
               [ IsVirtualHilbertPolynomialRep, IsRat ],

   function( virtual_hilbert_polynomial, rational )

     return VirtualHilbertPolynomial( UnderlyingPolynomial( virtual_hilbert_polynomial ) * rational );

end );

##
InstallMethod( \*,
               "for a rational number and a virtual Hilbert polynomial",
               [ IsRat, IsVirtualHilbertPolynomialRep ],

   function( rational, virtual_hilbert_polynomial )

     return VirtualHilbertPolynomial( rational * UnderlyingPolynomial( virtual_hilbert_polynomial ) );

end );

## TODO
##
InstallMethod( RowOfCoefficients,
               "for a polynomial and an integer",
               [ IsLaurentPolynomial, IsInt ],

  function( polynomial, degree )
    local coefficients_of_laurent_polynomial, first_degree, exponents_with_value, row_of_coefficients, i;

    coefficients_of_laurent_polynomial := CoefficientsOfLaurentPolynomial( polynomial )[ 1 ];

    first_degree := CoefficientsOfLaurentPolynomial( polynomial )[ 2 ];

    exponents_with_value := [ first_degree .. first_degree + Length( coefficients_of_laurent_polynomial ) - 1 ];

    row_of_coefficients := [ ];

    for i in [ 0 .. degree ] do

      if i in exponents_with_value then

        row_of_coefficients[ i + 1 ] := coefficients_of_laurent_polynomial[ i + 1 - first_degree ];

      else

        row_of_coefficients[ i + 1 ] := 0;

      fi;

    od;

    return row_of_coefficients;

end );              

## this should be an Attribute
##
InstallMethod( Degree,
               "for a virtual Hilbert polynomial",
               [ IsVirtualHilbertPolynomialRep ],

  function( virtual_hilbert_polynomial )

    return Degree( UnderlyingPolynomial( virtual_hilbert_polynomial ) );

end );

##
InstallMethod( \<,
               "for root sequences",
               [ IsRootSequenceRep, IsRootSequenceRep ],
               
  function( root_sequence_1, root_sequence_2 )
    local poset_of_root_sequences, i, is_root_sequence_2_strictly_bigger_in_one_component;
    
    poset_of_root_sequences := UnderlyingPosetOfRootSequences( root_sequence_1 );
    
    if not IsIdenticalObj( poset_of_root_sequences, UnderlyingPosetOfRootSequences( root_sequence_2 ) ) then
    
      Error( "root sequences are not defined over the identical partially ordered set of root sequences\n" );
    
    fi;

    is_root_sequence_2_strictly_bigger_in_one_component := false;
    
    for i in [ 1 .. LengthOfRootSequences( poset_of_root_sequences ) ] do
      
      if root_sequence_1!.StrictlyDecreasingIntegerSequence[ i ] > root_sequence_2!.StrictlyDecreasingIntegerSequence[ i ] then
        
        return false;
        
      elif not is_root_sequence_2_strictly_bigger_in_one_component and root_sequence_1!.StrictlyDecreasingIntegerSequence[ i ] < root_sequence_2!.StrictlyDecreasingIntegerSequence[ i ] then
      
        is_root_sequence_2_strictly_bigger_in_one_component := true;
      
      fi;
      
    od;
    
    return is_root_sequence_2_strictly_bigger_in_one_component; #and true

end );

##
InstallMethod( \=,
               "for root sequences",
               [ IsRootSequenceRep, IsRootSequenceRep ],

  function( root_sequence_1, root_sequence_2 )
    local poset_of_root_sequences, i;

    poset_of_root_sequences := UnderlyingPosetOfRootSequences( root_sequence_1 );

    if not IsIdenticalObj( poset_of_root_sequences, UnderlyingPosetOfRootSequences( root_sequence_2 ) ) then

      return false;
      
    fi;

    for i in [ 1 .. LengthOfRootSequences( poset_of_root_sequences ) ] do

      if not root_sequence_1!.StrictlyDecreasingIntegerSequence[ i ] = root_sequence_2!.StrictlyDecreasingIntegerSequence[ i ] then

        return false;

      fi;

    od;

    return true;

end );

##
InstallMethod( \<,
               "for intervals of root sequences",
               [ IsIntervalOfRootSequencesRep, IsIntervalOfRootSequencesRep ],

  function( interval_of_root_sequences_1, interval_of_root_sequences_2 )

    return ( LeftBoundary( interval_of_root_sequences_1 ) > LeftBoundary( interval_of_root_sequences_2 ) and
           RightBoundary( interval_of_root_sequences_1 ) <= RightBoundary( interval_of_root_sequences_2 ) )
           or
           ( LeftBoundary( interval_of_root_sequences_1 ) >= LeftBoundary( interval_of_root_sequences_2 ) and
           RightBoundary( interval_of_root_sequences_1 ) < RightBoundary( interval_of_root_sequences_2 ) );
           
end );

##
InstallMethod( \=,
               "for a pair of intervals of root sequences",
               [ IsIntervalOfRootSequencesRep, IsIntervalOfRootSequencesRep ],

  function( interval_of_root_sequences_1, interval_of_root_sequences_2 )

    return ( LeftBoundary( interval_of_root_sequences_1 ) = LeftBoundary( interval_of_root_sequences_2 ) ) and
           ( RightBoundary( interval_of_root_sequences_1 ) = RightBoundary( interval_of_root_sequences_2 ) );

end );

##
InstallMethod( \[\],
               "for a root sequence and an integer",
               [ IsRootSequenceRep, IsInt ],
               
  function( root_sequence, i )

    return root_sequence!.StrictlyDecreasingIntegerSequence[ i ];
  
end );

##
InstallMethod( \[\],
               "for a root sequence and a pair of integers",
               [ IsRootSequenceRep, IsList ],
               
  function( root_sequence, list )
    local i, D, n, j, hilbert_polynomial, value;

    i := list[ 1 ];

    D := list[ 2 ];

    n := LengthOfRootSequences( UnderlyingPosetOfRootSequences( root_sequence ) );

    hilbert_polynomial := HilbertPolynomial( root_sequence );

    if ( i = 0 and D > root_sequence[ i + 1 ] ) or ( i = n and D < root_sequence[ i ] ) or ( i in [ 1 .. n - 1 ] and D > root_sequence[ i + 1 ] and D < root_sequence[ i ] ) then

      value :=  AbsInt( Value( hilbert_polynomial, D ) );

    else

      value := 0;

    fi;

    return value;
  
end );

##
InstallMethod( \[\],
               "for a virtual cohomology table and a pair of integers",
               [ IsCombinatorialVirtualCohomologyTableRep, IsList ],
               
  function( virtual_cohomology_table, list )
    local i, D;
    
    i := list[ 1 ];
    
    D := list[ 2 ];
    
    return Sum( List( virtual_cohomology_table!.LinearCombinationOfRootSequences, l -> l[ 1 ] * l[ 2 ][ [ i, D ] ] ) );
    
end );

##
InstallMethod( \+,
               "for a pair of virtual cohomology tables",
               [ IsCombinatorialVirtualCohomologyTableRep, IsCombinatorialVirtualCohomologyTableRep ],

  function( virtual_cohomology_table1, virtual_cohomology_table2 )

    return VirtualCohomologyTable(
      Concatenation( virtual_cohomology_table1!.LinearCombinationOfRootSequences, virtual_cohomology_table2!.LinearCombinationOfRootSequences ) );

end );

##
InstallMethod( \*,
               "for a virtual cohomology table and a rational number",
               [ IsCombinatorialVirtualCohomologyTableRep, IsRat ],

   function( virtual_cohomology_table, rational )

     return VirtualCohomologyTable(
       List( virtual_cohomology_table!.LinearCombinationOfRootSequences, summand -> [ summand[ 1 ] * rational, summand[ 2 ] ] ) );

end );

##
InstallMethod( \*,
               "for a rational number and a virtual cohomology table",
               [ IsRat, IsCombinatorialVirtualCohomologyTableRep ],

   function( rational, virtual_cohomology_table )

      return VirtualCohomologyTable(
        List( virtual_cohomology_table!.LinearCombinationOfRootSequences, summand -> [ rational * summand[ 1 ], summand[ 2 ] ] ) );

end );

##
InstallMethod( SetDisplayInterval,
               "for a list",
               [ IsList ],

  function( list )

    BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.display_interval := list;

end );

##
InstallMethod( \in,
              "for a root sequence and a poset of root sequences",
              [ IsRootSequenceRep, IsPosetOfRootSequencesRep ],

  function( root_sequence, poset_of_root_sequences )
  
    return IsIdenticalObj( UnderlyingPosetOfRootSequences( root_sequence ), poset_of_root_sequences );
  
end );

##
InstallMethod( AllCohomologyTables,
               "for an interval of root sequences and a virtual Hilbert polynomial",
               [ IsIntervalOfRootSequences, IsVirtualHilbertPolynomial ],
               
  function( interval_of_root_sequences, virtual_hilbert_polynomial )
    local length_of_root_sequences, set_of_root_sequences, candidate_for_integral_point, quotient,
    ambient_space_of_virtual_hilbert_polynomials, embedding, homalg_matrix, row, solution, homalg_map, point_on_hyperplane,
    list_of_rays, ray, ambient_space_of_cone, cone, inequalities,
    top_maximal_hilbert_chain, equations_of_hyperplane, i,
    right_side_of_equations_of_hyperplane, number_of_equations, coefficient, all_cohomology_tables;

    length_of_root_sequences := LengthOfRootSequences( UnderlyingPosetOfRootSequences( interval_of_root_sequences ) );
    
    set_of_root_sequences := UnderlyingSet( interval_of_root_sequences );

    #0.step: handle the special case where the interval consists of one element

    if Length( set_of_root_sequences ) = 1 then

      candidate_for_integral_point := CohomologyTable( set_of_root_sequences[ 1 ] );
      
      quotient := UnderlyingPolynomial( virtual_hilbert_polynomial )/ HilbertPolynomial( candidate_for_integral_point );

      if IsConstantRationalFunction( quotient ) and
         IsIntegral( CoefficientsOfLaurentPolynomial( quotient )[ 1 ][ 1 ] * candidate_for_integral_point ) then

        return [ [ CoefficientsOfLaurentPolynomial( quotient )[ 1 ][ 1 ] * candidate_for_integral_point ], [ ], [ ] ];

      else

        return [ [ ], [ ], [ ] ];

      fi;

    fi;

    #1. step:
    #check whether the virtual Hilbert polynomial lies in the vector space associated to the interval of root sequences
    #and if so, compute a linear combination.

    ambient_space_of_virtual_hilbert_polynomials := VectorSpaceOfVirtualHilbertPolynomials( interval_of_root_sequences );
    
    embedding := EmbeddingIntoSuperVectorSpace( ambient_space_of_virtual_hilbert_polynomials );

    # the standard basis 1, x, ..., x^length_of_root_sequences
    SetPositionOfTheDefaultPresentation( Range( UnderlyingMorphism( embedding ) ), 1 );

    #TODO: implement SetToConcretePosition
    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( ambient_space_of_virtual_hilbert_polynomials ),
                                         PositionOfConcretePresentation( ambient_space_of_virtual_hilbert_polynomials ) );
    
    homalg_matrix := MatrixOfMap( UnderlyingMorphism( embedding ) );
    
    row :=
      HomalgMatrix(
        RowOfCoefficients( UnderlyingPolynomial( virtual_hilbert_polynomial ), length_of_root_sequences ),
        1, length_of_root_sequences + 1,
        BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.QQ
      );

    solution := RightDivide( row, homalg_matrix );

    if solution = false then

      return [ [ ], [ ], [ ] ];
    
    fi;

    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( ambient_space_of_virtual_hilbert_polynomials ),
                                         PositionOfConcretePresentation( ambient_space_of_virtual_hilbert_polynomials ) );

    homalg_map := HomalgMap( solution,
                             HomalgFreeLeftModule( 1, BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.QQ ),
                             UnderlyingVectorSpace( ambient_space_of_virtual_hilbert_polynomials ) );

    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( ambient_space_of_virtual_hilbert_polynomials ),
                                         ambient_space_of_virtual_hilbert_polynomials!.LatticePosition );

    point_on_hyperplane :=
      Concatenation( EntriesOfHomalgMatrix( MatrixOfMap( homalg_map ) ),
                     List( [ Dimension( ambient_space_of_virtual_hilbert_polynomials ) + 1 .. Length( TopMaximalChain( interval_of_root_sequences ) ) ], i -> 0 ) );
    
    #2. step: create the (inequalitieslength_of_root_sequences + 1 defining the ) cone of all cohomology tables in the vector space associated to the
    #interval given in the input

    list_of_rays := List( set_of_root_sequences, IntervalOfRootSequences );

    list_of_rays := List( list_of_rays, interval -> MorphismOfIntervalsOfRootSequences( interval, interval_of_root_sequences ) );

    list_of_rays := List( list_of_rays, VectorSpaceOfVirtualCohomologyTables );

    for ray in list_of_rays do

      SetPositionOfTheDefaultPresentation( Source( UnderlyingMorphism( ray ) ), PositionOfConcretePresentation( Source( ray ) ) );

    od;

    ambient_space_of_cone := VectorSpaceOfVirtualCohomologyTables( interval_of_root_sequences );

    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( ambient_space_of_cone ),
                                         ambient_space_of_cone!.LatticePosition );

    list_of_rays := List( list_of_rays, ray ->  EntriesOfHomalgMatrix( MatrixOfMap( UnderlyingMorphism( ray ) ) ) );

    list_of_rays := List( list_of_rays, ray -> Lcm( List( ray, DenominatorRat ) ) * ray );

    cone := Cone( list_of_rays );

    inequalities := DefiningInequalities( cone );

    #3. step: create the subspace of all cohomology tables with vanishing Hilbert polynomial

    top_maximal_hilbert_chain := TopMaximalHilbertChain( interval_of_root_sequences );

    equations_of_hyperplane := [ ];
    
    for i in [ 1 .. Length( top_maximal_hilbert_chain ) ] do

      row := List( [ 1 .. Dimension( ambient_space_of_cone ) ], i -> 0 );

      row[ i ] := 1;
      
      Add( equations_of_hyperplane, row );

    od;

    #4. step:

    right_side_of_equations_of_hyperplane :=
      List( equations_of_hyperplane, equation -> ScalarProduct( equation, point_on_hyperplane ) );

    number_of_equations := Length( equations_of_hyperplane );

    for i in [ 1 .. number_of_equations ] do

      coefficient :=
        Lcm( List( Concatenation( equations_of_hyperplane[ i ], [ right_side_of_equations_of_hyperplane[ i ] ] ), DenominatorRat ) );

      equations_of_hyperplane[ i ] := equations_of_hyperplane[ i ] * coefficient;

      right_side_of_equations_of_hyperplane[ i ] := right_side_of_equations_of_hyperplane[ i ] * coefficient;

    od;
    
    all_cohomology_tables :=
      4ti2Interface_zsolve_equalities_and_inequalities( equations_of_hyperplane, right_side_of_equations_of_hyperplane,
      inequalities, List( inequalities, i -> 0 ) );

    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( ambient_space_of_cone ),
                                         ambient_space_of_cone!.LatticePosition );
  
    all_cohomology_tables :=
      List( all_cohomology_tables, sublist ->
        List( sublist , row ->
          HomalgMap( [ row ], HomalgFreeLeftModule( 1, BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.QQ ),
          UnderlyingVectorSpace( ambient_space_of_cone ), BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.QQ )
        )
      );

    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( ambient_space_of_cone ),
                                         PositionOfConcretePresentation( ambient_space_of_cone ) );

    all_cohomology_tables :=
      List( all_cohomology_tables, sublist ->
        List( sublist, map -> EntriesOfHomalgMatrix( MatrixOfMap( map ) ) )
      );

    all_cohomology_tables :=
      List( all_cohomology_tables, sublist ->
        List( sublist, row -> VirtualCohomologyTable( row, interval_of_root_sequences ) )
      );
      
    return all_cohomology_tables;
    
end );

##
InstallMethod( AllHilbertPolynomials,
               "for an interval of root sequences and an integer",
               [ IsIntervalOfRootSequences, IsInt, IsBool ],
  function( interval_of_root_sequences, rank, filter )
    local set_of_root_sequences, candidate_for_integral_point, list_of_rays, ray, cone, inequalities,
    length_of_root_sequences, vector_space_of_virtual_hilbert_polynomials, ambient_space_of_cone,
    projection, kernel_embedding, ray_generators_of_subspace, equations_of_hyperplane,
    coefficient, i, ith_vector_position, point_on_hyperplane,
    right_side_of_equations_of_hyperplane, number_of_equations, all_hilbert_polynomials;

    set_of_root_sequences := UnderlyingSet( interval_of_root_sequences );
    
    #0. step: handle the special case where the interval consists of one element.
    
    if Length( set_of_root_sequences ) = 1 then

      candidate_for_integral_point := VirtualHilbertPolynomial( set_of_root_sequences[ 1 ] ) * rank;

      if IsIntegral( candidate_for_integral_point ) then

        return [ [ TwistedChernPolynomial( candidate_for_integral_point ), candidate_for_integral_point ], [ ], [ ] ];

      else

        return [ [ ], [ ], [ ] ];

      fi;
    
    fi;
    
    #1. step: create the (inequalities defining the ) cone of all Hilbert polynomials in the vector space associated to the
    #interval given in the input

    list_of_rays := List( set_of_root_sequences, IntervalOfRootSequences );

    list_of_rays := List( list_of_rays, interval -> MorphismOfIntervalsOfRootSequences( interval, interval_of_root_sequences ) );

    list_of_rays := List( list_of_rays, VectorSpaceOfVirtualHilbertPolynomials );

    for ray in list_of_rays do

      SetPositionOfTheDefaultPresentation( Source( UnderlyingMorphism( ray ) ), PositionOfConcretePresentation( Source( ray ) ) );

    od;

    ambient_space_of_cone := VectorSpaceOfVirtualHilbertPolynomials( interval_of_root_sequences );

    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( ambient_space_of_cone ),
                                         ambient_space_of_cone!.LatticePosition );
                                         
    list_of_rays := List( list_of_rays, ray ->  EntriesOfHomalgMatrix( MatrixOfMap( UnderlyingMorphism( ray ) ) ) );

    list_of_rays := List( list_of_rays, ray -> Lcm( List( ray, DenominatorRat ) ) * ray );

    cone := Cone( list_of_rays );

    inequalities := DefiningInequalities( cone );

    #2. step: create the subspace of all Hilbert polynomials of degree < length_of_root_sequences ( <=> rank = 0 )
    length_of_root_sequences := LengthOfRootSequences( UnderlyingPosetOfRootSequences( interval_of_root_sequences ) );
    
    vector_space_of_virtual_hilbert_polynomials :=
      UnderlyingVectorSpace(
        BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.FullVectorSpaceOfVirtualHilbertPolynomials[ length_of_root_sequences ]
      );

    # the standard basis 1, x, ..., x^length_of_root_sequences
    SetPositionOfTheDefaultPresentation( vector_space_of_virtual_hilbert_polynomials, 1 );

    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( ambient_space_of_cone ),
                                         ambient_space_of_cone!.LatticePosition );    
    
    projection := List( [ 1 .. length_of_root_sequences ], i -> 0 );
    
    Add( projection, 1 );
    
    projection := HomalgMap( projection,
                    ambient_space_of_cone,
                    HomalgFreeLeftModule( length_of_root_sequences + 1, BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ ),
                    BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ );
    
    kernel_embedding := KernelEmb( PreCompose(
                                     UnderlyingMorphism( EmbeddingIntoSuperVectorSpace( ambient_space_of_cone ) ),
                                     projection )
                                 );

    if not IsZero( kernel_embedding ) then
    
      kernel_embedding := Lcm( List( EntriesOfHomalgMatrix( MatrixOfMap( kernel_embedding ) ), DenominatorRat ) ) * kernel_embedding;

      ray_generators_of_subspace := EntriesOfHomalgMatrixAsListList( MatrixOfMap( kernel_embedding ) );

      ray_generators_of_subspace := Concatenation( ray_generators_of_subspace, -ray_generators_of_subspace );

      #the result is not mutable
      equations_of_hyperplane := DefiningInequalities( Cone( ray_generators_of_subspace ) );

      #IsMutable -> true?
      equations_of_hyperplane := ShallowCopy( equations_of_hyperplane );

    else

      equations_of_hyperplane :=
        IdentityMat( Dimension( ambient_space_of_cone ) );
    fi;

    #3. step: compute the right side of the equations of the hyperplane

    # the standard basis 1, x, ..., x^length_of_root_sequences
    SetPositionOfTheDefaultPresentation( vector_space_of_virtual_hilbert_polynomials, 1 );

    coefficient :=
      EntriesOfHomalgMatrixAsListList(
        MatrixOfMap(
          UnderlyingMorphism( EmbeddingIntoSuperVectorSpace( ambient_space_of_cone ) )
        )
      );
    
    for i in [ 1 .. Length( coefficient ) ] do

      if coefficient[ i ][ length_of_root_sequences + 1 ] <> 0 then
      
        coefficient := coefficient[ i ];

        ith_vector_position := i;
        
        break;
      
      fi;
    
    od;
    
    if IsList( coefficient[ 1 ] ) then

      Error( "There is no rational point on the hyperplane of Hilbert polynomials with given rank. This is either a bug or a mathematical error\n" );

    fi;
    
    coefficient := rank / ( coefficient[ length_of_root_sequences + 1 ] * Factorial( length_of_root_sequences ) );

    point_on_hyperplane := List( [ 1 .. Dimension( ambient_space_of_cone ) ], i -> 0 );

    point_on_hyperplane[ ith_vector_position ] := coefficient;

    right_side_of_equations_of_hyperplane :=
      List( equations_of_hyperplane, equation -> ScalarProduct( equation, point_on_hyperplane ) );

    number_of_equations := Length( equations_of_hyperplane );
      
    for i in [ 1 .. number_of_equations ] do

      coefficient :=
        Lcm( List( Concatenation( equations_of_hyperplane[ i ], [ right_side_of_equations_of_hyperplane[ i ] ] ), DenominatorRat ) );

      equations_of_hyperplane[ i ] := equations_of_hyperplane[ i ] * coefficient;

      right_side_of_equations_of_hyperplane[ i ] := right_side_of_equations_of_hyperplane[ i ] * coefficient;
      
    od;

    
    #4. step : compute the intersection of the Boij Söderberg cone with the hyperplane

    all_hilbert_polynomials :=
      4ti2Interface_zsolve_equalities_and_inequalities( equations_of_hyperplane, right_side_of_equations_of_hyperplane,
      inequalities, List( inequalities, i -> 0 ) );

    all_hilbert_polynomials :=
      List( all_hilbert_polynomials , sublist ->
        List( sublist,
          vector ->
            PreCompose(
              HomalgMap( [ vector ],
                HomalgFreeLeftModule( 1,
                BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ ),
                UnderlyingVectorSpace( ambient_space_of_cone ),
                BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ ),
              UnderlyingMorphism( EmbeddingIntoSuperVectorSpace( ambient_space_of_cone ) )
             )
        )
    );

    # the standard basis 1, x, ..., x^length_of_root_sequences
    SetPositionOfTheDefaultPresentation( vector_space_of_virtual_hilbert_polynomials, 1 );

    all_hilbert_polynomials :=
      List( all_hilbert_polynomials, sublist ->
        List( sublist, list_of_coefficients ->
          VirtualHilbertPolynomial( EntriesOfHomalgMatrix( MatrixOfMap( list_of_coefficients ) ) )
        )
      );

    if filter then

      all_hilbert_polynomials :=
        List( all_hilbert_polynomials, sublist ->
          List( sublist, virtual_hilbert_polynomial ->
            [ TwistedChernPolynomial( virtual_hilbert_polynomial ), virtual_hilbert_polynomial ]
          )
        );

     all_hilbert_polynomials :=
        List( all_hilbert_polynomials, sublist ->
          Filtered( sublist, chern_polynomial ->
            Degree( TotalChernClass( chern_polynomial[ 1 ][ 1 ] )!.polynomial ) <= rank
          )
        );
    
    fi;
                    
    return all_hilbert_polynomials;
    
end );

##
InstallMethod( AllHilbertPolynomials,
               "for an interval of root sequences and an integer",
               [ IsIntervalOfRootSequences, IsInt ],
  function( interval_of_root_sequences, rank )

    return AllHilbertPolynomials( interval_of_root_sequences, rank, true );

end );

##################################
##
## Constructors
##
##################################

##
InstallMethod(  PosetOfRootSequences,
               "for an integer",
               [ IsInt ],
               
  function( length_of_root_sequences )
    local poset_of_root_sequences;
    
    poset_of_root_sequences := rec( );
    
    ObjectifyWithAttributes( poset_of_root_sequences, TheTypePosetOfRootSequences,
                             LengthOfRootSequences, length_of_root_sequences
                           );
              
    return poset_of_root_sequences;

end );

##
InstallGlobalFunction( RootSequence,
  function( arg )
    local nargs, list, integer_sequence, n, poset_of_root_sequences, root_sequence;

    nargs := Length( arg );

    if not nargs in [ 1, 2 ] then

      Error( "arguments must either be [ IsList ] or [ IsList, IsPosetOfRootSequences ]\n" );

    fi;

    list := arg[ 1 ];

    if not IsList( list ) then

      Error( "the first argument must be a list\n" );

    fi;

    if false in List( list, IsInt ) then

      Error( "the list must only contain integers\n" );

    fi;

    integer_sequence := Reversed( Set( list ) );

    if nargs = 1 then

      n := Length( integer_sequence );
  
      if not IsBound( BOIJ_SOEDERBERG.PosetOfRootSequences[ n ] ) then
      
        BOIJ_SOEDERBERG.PosetOfRootSequences[ n ] := PosetOfRootSequences( n );

      fi;

      poset_of_root_sequences := BOIJ_SOEDERBERG.PosetOfRootSequences[ n ];
    
    else

      poset_of_root_sequences := arg[ 2 ];
    
      if not IsPosetOfRootSequencesRep( poset_of_root_sequences ) then

        Error( "the second argument must be a poset of root sequences\n" );
      
      fi;

      n := LengthOfRootSequences( poset_of_root_sequences );

      if Length( integer_sequence ) <> n then
        
        Error( Concatenation( "the list does not represent an element of a partially ordered set of root sequences of length ", String( n ), "\n" ) );
        
      fi;
    
    fi; 
    
    root_sequence := rec( StrictlyDecreasingIntegerSequence := integer_sequence );

    ObjectifyWithAttributes( root_sequence, TheTypeRootSequence,
                             UnderlyingPosetOfRootSequences, poset_of_root_sequences
                           );
    
    return root_sequence;

end );

##
InstallMethod( IntervalOfRootSequences,
               "for a pair of root sequences",
               [ IsRootSequenceRep, IsRootSequenceRep ],
               
  function( root_sequence_1, root_sequence_2 )
    local interval_of_root_sequences;

    if not root_sequence_1 <= root_sequence_2 then

      Error( "the first root sequence must be lower or equal to the second root sequence\n" );
    
    fi;
    
    interval_of_root_sequences := rec( );

    ObjectifyWithAttributes( interval_of_root_sequences, TheTypeIntervalOfRootSequences,
                             UnderlyingPosetOfRootSequences, UnderlyingPosetOfRootSequences( root_sequence_1 ),
                             LeftBoundary, root_sequence_1,
                             RightBoundary, root_sequence_2,
                             Category, BOIJ_SOEDERBERG.CategoryOfIntervalsOfRootSequences
                           );

    return interval_of_root_sequences;

end );

##
InstallMethod( IntervalOfRootSequences,
               "for a root sequence",
               [ IsRootSequenceRep ],

  function( root_sequence )

    return IntervalOfRootSequences( root_sequence, root_sequence );

end );


##
InstallMethod( MorphismOfIntervalsOfRootSequences,
               "for a pair of intervals of root sequences",
               [ IsIntervalOfRootSequences, IsIntervalOfRootSequences ],

  function( interval_of_root_sequences_1, interval_of_root_sequences_2 )
    local morphism_of_intervals_of_root_sequences;

    if not ( interval_of_root_sequences_1 <= interval_of_root_sequences_2 ) then

      Error( "the first interval must be contained in the second interval\n" );

    fi;

    morphism_of_intervals_of_root_sequences := rec( );

    ObjectifyWithAttributes( morphism_of_intervals_of_root_sequences, TheTypeMorphismOfIntervalsOfRootSequences,
                             Source, interval_of_root_sequences_1,
                             Range, interval_of_root_sequences_2
                           );

    return morphism_of_intervals_of_root_sequences;
               
end );


##
InstallMethod( VirtualCohomologyTable,
               "for a list",
               [ IsList ],
  function( list )
    local l, LinearCombinationOfRootSequences, poset_of_root_sequences, reduced_list,
    summand, position, virtual_cohomology_table;
    
    for l in list do
      
      if not IsList( l ) or not IsRat( l[1] ) or not IsRootSequence( l[2] ) then

        Error( "the list does not consist of pairs of rational numbers and root sequences\n" );

      fi;
      
    od;

    poset_of_root_sequences := UnderlyingPosetOfRootSequences( list[1][2] );

    for l in list{ [ 2 .. Length( list ) ] } do

      if not IsIdenticalObj( poset_of_root_sequences, UnderlyingPosetOfRootSequences( l[2] ) ) then

        Error( "the root sequences are not defined over the identical poset of root sequences\n" );

      fi;
    
    od;

    reduced_list := [ ];

    for summand in list do

      position := PositionProperty( reduced_list, reduced_summand -> reduced_summand[ 2 ] = summand[ 2 ] );

      if not position = fail then

        reduced_list[ position ] := [ reduced_list[ position ][ 1 ] + summand[ 1 ], summand[ 2 ] ];

      else

        Add( reduced_list, summand );

      fi;
    
    od;

    reduced_list := Filtered( reduced_list, summand -> summand[ 1 ] <> 0 );
    
    virtual_cohomology_table := rec( LinearCombinationOfRootSequences := reduced_list );

    ObjectifyWithAttributes( virtual_cohomology_table, TheTypeCombinatorialVirtualCohomologyTable,
                             UnderlyingPosetOfRootSequences, poset_of_root_sequences
                           );

    return virtual_cohomology_table;
    
end );

##
InstallMethod( VirtualCohomologyTable,
               "for a list and an interval of root sequences",
               [ IsList, IsIntervalOfRootSequencesRep ],

  function( list_of_coefficients, interval_of_root_sequences )
    local top_maximal_chain, list;

    top_maximal_chain := TopMaximalChain( interval_of_root_sequences );

    if Length( top_maximal_chain ) <> Length( list_of_coefficients ) then

      Error( "the dimension does not match the number of elements of the first list" );
    
    fi;
    
    list := List( [ 1 .. Length( top_maximal_chain ) ] ,
              i -> [ list_of_coefficients[ i ], top_maximal_chain[ i ] ] );

    return VirtualCohomologyTable( list );
  
end );

##
InstallMethod( VirtualHilbertPolynomial,
               "for a list of rationals",
               [ IsList ],

  function( list_of_coefficients )
    local t, degree, virtual_hilbert_polynomial, underlying_polynomial;
    
    if false in List( list_of_coefficients, IsRat ) then

      Error( "the list must contain rational numbers" );
    
    fi;

    t := VariableForHilbertPolynomial( );

    degree := Length( list_of_coefficients ) - 1;

    underlying_polynomial := Sum( List( [ 0 .. degree ], i -> list_of_coefficients[ i + 1 ] * t^i ) );
  
    virtual_hilbert_polynomial := rec( ListOfCoefficients := list_of_coefficients );

    ObjectifyWithAttributes( virtual_hilbert_polynomial, TheTypeVirtualHilbertPolynomial,
                             UnderlyingPolynomial, underlying_polynomial
                           );

    return virtual_hilbert_polynomial;
  
end );

#TODO: find the correct representation
##
InstallMethod( VirtualHilbertPolynomial,
               "for a polynomial",
               [ IsLaurentPolynomial ],

  function( polynomial )
  
    return VirtualHilbertPolynomial( RowOfCoefficients( polynomial, Degree( polynomial ) ) );
  
end );

##
InstallMethod( VirtualHilbertPolynomial,
               "for a root sequence",
               [ IsRootSequenceRep ],

  function( root_sequence )
  
    return VirtualHilbertPolynomial( HilbertPolynomial( root_sequence ) );

end );

##
InstallMethod( VirtualHilbertPolynomial,
               "for a virtual_cohomology_table",
               [ IsVirtualCohomologyTableRep ],

  function( virtual_cohomology_table )

    return VirtualHilbertPolynomial( HilbertPolynomial( virtual_cohomology_table ) );
  
end );
               
##
InstallMethod( VectorSpaceWithIntegralStructure,
               "for an isomorphism of vectorspaces",
               [ IsMapOfFinitelyGeneratedModulesRep ],
  function( homalg_map )
    local vector_space_with_integral_structure, underlying_vector_space;

    if not HasIsIsomorphism( homalg_map ) then

      Error( "the map does not know if it is an isomorphism\n" );

    elif not IsIsomorphism( homalg_map ) then

      Error( "the map is not an isomorphism\n" );

    fi;

    if not IsFieldForHomalg( HomalgRing( homalg_map ) ) then

      Error( "the map is not a map of vector spaces\n" );

    fi;

    PushPresentationByIsomorphism( homalg_map );

    underlying_vector_space := Range( homalg_map );

    vector_space_with_integral_structure := rec( LatticePosition := underlying_vector_space!.PositionOfTheDefaultPresentation );

    ObjectifyWithAttributes( vector_space_with_integral_structure, TheTypeVectorSpaceWithIntegralStructure,
                             UnderlyingStructureMorphism, homalg_map,
                             UnderlyingVectorSpace, underlying_vector_space#,
#                              UnderlyingHomalgRing, HomalgRing( homalg_map )
                           );

    return vector_space_with_integral_structure;
 
end );

##
InstallMethod( SubVectorSpaceOfHilbertPolynomials,
               "for a list and an integer",
               [ IsList, IsInt ],

  function( list_of_hilbert_polynomials, degree )
    local full_vector_space_of_virtual_hilbert_polynomials, coefficients_matrix, homalg_free_left_module, embedding_as_vector_spaces,
          embedding, vector_space_with_integral_structure;
  
    if false in List( list_of_hilbert_polynomials, IsUnivariatePolynomial ) then

      Error( "the first argument must be a list of univariate polynomials\n" );
      
    fi;

    full_vector_space_of_virtual_hilbert_polynomials :=
      FullVectorSpaceOfVirtualHilbertPolynomials( degree );
      
    #the standard basis ( 1, x, x^2, ... ) is in position 1 by definition
    SetPositionOfTheDefaultPresentation( UnderlyingVectorSpace( full_vector_space_of_virtual_hilbert_polynomials ), 1 );
    
    coefficients_matrix := List( list_of_hilbert_polynomials, polynomial -> RowOfCoefficients( polynomial, degree ) );

    homalg_free_left_module := HomalgFreeLeftModule( Length( list_of_hilbert_polynomials ), BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ );
    
    embedding_as_vector_spaces := HomalgMap( coefficients_matrix,
                                             homalg_free_left_module,
                                             UnderlyingVectorSpace( full_vector_space_of_virtual_hilbert_polynomials ),
                                             BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualHilbertPolynomials!.QQ
                                            );
                                            
    if not IsMonomorphism( embedding_as_vector_spaces ) then

      Error( "the given polynomials are not linearly independent\n" );
    
    fi;                     

    embedding := MorphismOfVectorSpacesWithIntegralStructure( full_vector_space_of_virtual_hilbert_polynomials, embedding_as_vector_spaces );

    vector_space_with_integral_structure := Source( embedding );

    SetIsEmbedded( vector_space_with_integral_structure, true );

    SetEmbeddingIntoSuperVectorSpace( vector_space_with_integral_structure, embedding );

    SetIsConcrete( vector_space_with_integral_structure, true );

    SetPositionOfConcretePresentation( vector_space_with_integral_structure, 1 );

    return vector_space_with_integral_structure;

end );
               

##
InstallMethod( MorphismOfVectorSpacesWithIntegralStructure,
               "for a pair of vector spaces with integral structure and a homalg map",
               [ IsVectorSpaceWithIntegralStructure, IsVectorSpaceWithIntegralStructure, IsMapOfFinitelyGeneratedModulesRep ],
  function( vector_space_with_integral_structure1, vector_space_with_integral_structure2, homalg_map )
    local source, range, homalg_matrix, morphism_of_vector_spaces_with_integral_structure;

    source := UnderlyingVectorSpace( vector_space_with_integral_structure1 );

    range := UnderlyingVectorSpace( vector_space_with_integral_structure2 );
  
    if not IsIdenticalObj( source, Source( homalg_map ) ) or
       not IsIdenticalObj( range, Range( homalg_map ) ) then

      Error( "the underlying vector spaces are not identical to the source or range of the map\n" );
       
    fi;

    SetPositionOfTheDefaultPresentation( source,
                                         vector_space_with_integral_structure1!.LatticePosition );

    SetPositionOfTheDefaultPresentation( range,
                                         vector_space_with_integral_structure2!.LatticePosition );

    homalg_matrix := MatrixOfMap( homalg_map );
    
    if false in List( EntriesOfHomalgMatrix( homalg_matrix ), IsInt ) then

      Error( "the given map does not respect the integral structure\n" );
    
    fi;

    morphism_of_vector_spaces_with_integral_structure := rec( );

    ObjectifyWithAttributes( morphism_of_vector_spaces_with_integral_structure, TheTypeMorphismOfVectorSpacesWithIntegralStructure,
                             UnderlyingMorphism, homalg_map,
                             Source, vector_space_with_integral_structure1,
                             Range, vector_space_with_integral_structure2 );

    return morphism_of_vector_spaces_with_integral_structure;
  
end );

##
InstallMethod( MorphismOfVectorSpacesWithIntegralStructure,
               "for a vector space with integral structure and a monomorphism",
               [ IsVectorSpaceWithIntegralStructureRep, IsMapOfFinitelyGeneratedModulesRep ],

  function( vector_space_with_integral_structure, homalg_map )
    local range, lcm_of_denominators, smith_normal_form_of_integer_mat, diagonal_rescaling_mat, homalg_matrix,
          structure_morphism_of_source, source, morphism_of_vector_spaces_with_integral_structure;

    if not HasIsMonomorphism( homalg_map ) then

      Error( "the map does not know if it is a monomorphism\n" );

    elif not IsMonomorphism( homalg_map ) then

      Error( "the map is not a monomorphism\n" );

    fi;
    
    range := UnderlyingVectorSpace( vector_space_with_integral_structure );

    if not IsIdenticalObj( range, Range( homalg_map ) ) then

      Error( "the underlying vector space is not identical to the range of the map\n" );

    fi;
    
    SetPositionOfTheDefaultPresentation( range,
                                         vector_space_with_integral_structure!.LatticePosition );

    homalg_matrix := MatrixOfMap( homalg_map );

    # is SmithNormalForm also available in Homalg?
    lcm_of_denominators := Lcm( List( EntriesOfHomalgMatrix( homalg_matrix ), DenominatorRat ) );

    smith_normal_form_of_integer_mat := SmithNormalFormIntegerMatTransforms( lcm_of_denominators * EntriesOfHomalgMatrixAsListList( homalg_matrix ) );

    diagonal_rescaling_mat := DiagonalMat( List( [ 1 .. Length( smith_normal_form_of_integer_mat.normal ) ], i -> 1 / smith_normal_form_of_integer_mat.normal[ i ][ i ] ) );

    structure_morphism_of_source := HomalgMap(
                                               diagonal_rescaling_mat * smith_normal_form_of_integer_mat.rowtrans * lcm_of_denominators,
                                               Source( homalg_map ), Source( homalg_map ), HomalgRing( homalg_map )
                                             );

    if not IsIsomorphism( structure_morphism_of_source ) then

      Error( "structure morphism of the input is not an isomorphism. This is either a bug or a mathematical error\n" );

    fi;
                                             
    source := VectorSpaceWithIntegralStructure( structure_morphism_of_source );
    
    morphism_of_vector_spaces_with_integral_structure := MorphismOfVectorSpacesWithIntegralStructure( source, vector_space_with_integral_structure, homalg_map );
    
    return morphism_of_vector_spaces_with_integral_structure;
    
end );
  
               
#################################
##
## Display
##
#################################

##
InstallMethod( ViewObj,
               "for partially ordered sets of root sequences",
               [ IsPosetOfRootSequencesRep ],
               
  function( poset_of_root_sequences )

    Print( "<" ); 
    
    Display( poset_of_root_sequences );
    
    Print( ">" );
        
end );

##
InstallMethod( Display,
               "for posets of root sequences",
               [ IsPosetOfRootSequencesRep ],

  function( poset_of_root_sequences )

    Print( "A poset of root sequences" );

    if HasLengthOfRootSequences( poset_of_root_sequences ) then
      
      Print( " of length ", LengthOfRootSequences( poset_of_root_sequences ) );

    fi;

end );

##
InstallMethod( ViewObj,
               "for a root sequence",
               [ IsRootSequenceRep ],
               
  function( root_sequence )

    Display( root_sequence );
    
end );

##
InstallMethod( Display,
               "for a root sequence",
               [ IsRootSequenceRep ],
               
  function( root_sequence )

    Print( root_sequence!.StrictlyDecreasingIntegerSequence );

end );

##
InstallMethod( ViewObj,
               "for an interval of root sequences",
               [ IsIntervalOfRootSequencesRep ],
               
  function( interval_of_root_sequences )

    Display( interval_of_root_sequences );

end );

##
InstallMethod( Display,
               "for an interval of root sequences",
               [ IsIntervalOfRootSequencesRep ],
               
  function( interval_of_root_sequences )

    Print( "[ " );

    Display( LeftBoundary( interval_of_root_sequences ) );

    Print( " .. " );

    Display( RightBoundary( interval_of_root_sequences ) );

    Print( " ]" );
    
end );

##
InstallMethod( ViewObj,
               "for a morphism of intervals of root sequences",
               [ IsMorphismOfIntervalsOfRootSequencesRep ],

  function( morphism_of_intervals_of_root_sequences )

    Display( morphism_of_intervals_of_root_sequences );

end );

##
InstallMethod( Display,
               "for a morphism of intervals of root sequences",
               [ IsMorphismOfIntervalsOfRootSequencesRep ],

  function( morphism_of_intervals_of_root_sequences )

    Display( Source( morphism_of_intervals_of_root_sequences ) );

    Print( " -> " );

    Display( Range( morphism_of_intervals_of_root_sequences ) );

end );

##
InstallMethod( ViewObj,
               "for virtual cohomology tables",
               [ IsVirtualCohomologyTableRep ],
               
  function( virtual_cohomology_table )

    Print( "<A virtual cohomology table>" );

end );

##
InstallMethod( Display,
               "for virtual cohomology tables",
               [ IsVirtualCohomologyTableRep ],
               
  function( virtual_cohomology_table )
    local betti_table;

    betti_table := BettiTable( virtual_cohomology_table );

    if betti_table!.column_range = BOIJ_SOEDERBERG.CategoryOfVectorSpacesOfVirtualCohomologyTables!.display_interval then
  
      Display( betti_table );

    else

      ResetFilterObj( virtual_cohomology_table, HasBettiTable );

      Display( BettiTable( virtual_cohomology_table ) );

    fi;

end );

##
InstallMethod( ViewObj,
               "for a virtual Hilbert polynomial",
               [ IsVirtualHilbertPolynomialRep ],

  function( virtual_hilbert_polynomial )

    ViewObj( UnderlyingPolynomial( virtual_hilbert_polynomial ) );
  
end );

##
InstallMethod( Display,
               "for a virtual Hilbert polynomial",
               [ IsVirtualHilbertPolynomialRep ],

  function( virtual_hilbert_polynomial )

    Display( UnderlyingPolynomial( virtual_hilbert_polynomial ) );

end );

##
InstallMethod( ViewObj,
               "for a vector space with integral structure",
               [ IsVectorSpaceWithIntegralStructureRep ],

  function( vector_space_with_integral_structure )

    Print( "<" );

    Display( vector_space_with_integral_structure );

    Print( ">" );

end );

##
InstallMethod( Display,
               "for a vector space with integral structure",
               [ IsVectorSpaceWithIntegralStructureRep ],

  function( vector_space_with_integral_structure )

    Print( "A vector space with integral structure of dimension ", Dimension( vector_space_with_integral_structure ) );

end );

##
InstallMethod( ViewObj,
               "for a morphism of vector spaces with integral structure",
               [ IsMorphismOfVectorSpacesWithIntegralStructureRep ],
               
  function( morphism_of_vector_spaces_with_integral_structure )

    ViewObj( Source( morphism_of_vector_spaces_with_integral_structure ) );

    Print( " -> " );

    ViewObj( Range( morphism_of_vector_spaces_with_integral_structure ) );

end );

##
InstallMethod( Display,
               "for a morphism of vector spaces with integral structure",
               [ IsMorphismOfVectorSpacesWithIntegralStructureRep ],

  function( morphism_of_vector_spaces_with_integral_structure )

    Print(  "A morphism of vector spaces with integral structure.\n" );
    
    Display( UnderlyingMorphism( morphism_of_vector_spaces_with_integral_structure ) );

end );


               

