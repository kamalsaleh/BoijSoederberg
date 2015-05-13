LoadPackage( "Kamal" );

R := HomalgFieldOfRationalsInSingular( ) * "x,y,z";
S := GradedRing( R );

M := LeftPresentationWithDegrees( HomalgMatrix( "[ x^2, x*y, z^2 ]", 3, 1, S ) );


