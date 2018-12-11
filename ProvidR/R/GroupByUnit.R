#groups by diferent units
GroupByUnit<-function(data, unit){
  if(unit=="zipcode"){
    countbyzip<-data%>%
      group_by(zipcode) %>%
      count() %>%
      arrange(n)
<<<<<<< HEAD
    
=======

>>>>>>> 8cbebc6441b8da355e39f4f449cddee018b55a05
    return(countbyzip)
  } else if (unit==="county") {
    countbycounty<-data%>%
      group_by(county) %>%
      count() %>%
      arrange(n)
<<<<<<< HEAD
    
=======

>>>>>>> 8cbebc6441b8da355e39f4f449cddee018b55a05
    return(countbycounty)
  } else if (unit=="state"){
    countbystate<-data%>%
      group_by(state) %>%
      count() %>%
      arrange(n)
<<<<<<< HEAD
    
=======

>>>>>>> 8cbebc6441b8da355e39f4f449cddee018b55a05
    return(countbystate)
  } else if (unit=="city"){
    countbycity<-data%>%
      group_by(city) %>%
      count() %>%
      arrange(n)
<<<<<<< HEAD
    
=======

>>>>>>> 8cbebc6441b8da355e39f4f449cddee018b55a05
    return(countbycity)
  } else if (unit=="type"){
    countbyprovider<-data%>%
      group_by(Primary_Taxonomy) %>%
      count() %>%
      arrange(desc(n))
<<<<<<< HEAD
    
    return(countbyprovider)
  }
}
 
=======

    return(countbyprovider)
  }
}
>>>>>>> 8cbebc6441b8da355e39f4f449cddee018b55a05
