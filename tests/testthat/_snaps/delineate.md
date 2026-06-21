# print.delineation output matches snapshot

    Code
      print(minimal_delineation)
    Output
      City:    TestCity
      River:   TestRiver
      CRS:     WGS 84 / UTM zone 35N 
      
      Delineation layers:
        $valley          1 feature
        $corridor        1 feature
        $segments        2 features
        $riverspace      -
      
      Base layers:
        $streets         1 feature
        $railways        -
        $river_centerline 1 feature
        $river_surface   1 feature

# summary.delineation and print.summary.delineation match snapshot

    Code
      summary(minimal_delineation)
    Output
      Delineation: TestCity - TestRiver 
      CRS:         WGS 84 / UTM zone 35N 
      
      Delineation parameters:
        network_buffer   3000 m
        dem_buffer       2500 m
        buildings_buffer 100 m
      
      Delineation layers:
        $valley          1.0 km²
        $corridor        1.0 km²
        $segments        2 features, total 2.0 km² (mean 1.0 km²)
        $riverspace      -
      
      Base layers:
        $streets         1 feature
        $river_centerline 2.0 km
        $river_surface   1.0 km²

