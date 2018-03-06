remove_na <- function(data) {
  data %>%
    dplyr::select(which(colMeans(is.na(.)) < 1))
  
}


# munge -------------------------------------------------------------------

.munge_realtor <- 
  function(data) {
   num_names <- data %>% dplyr::select(matches("^area|^count|^price|^latitude|^longitude")) %>% names()
   
   data <- 
     data %>% 
     mutate_at(num_names, 
               funs(. %>% readr::parse_number()))
   
   log_names <- data %>% dplyr::select(matches("^is|^has")) %>% names()
   
   data <- 
     data %>% 
     mutate_at(log_names,
               funs(as.logical))
   
   if (data %>% tibble::has_name("slugLDP")) {
    data <- 
      data %>%
      mutate(urlListing = glue::glue("https://www.realtor.com/{slugLDP}") %>% as.character()) %>%
      dplyr::select(-slugLDP)
   }
   data
  }

# dict --------------------------------------------------------------------

dictionary_realtor_names <- 
  function() {
    data_frame(nameRealtor = c("price_per_sqft", "median_listing_price", "median_rent_price",
                               "for_sale_count",
                               "new_listing_count",
                               "open_house_count",
                               "recently_sold_count",
                               "foreclosure_count",
                               "price_reduced_count",
                               "ldp_url", "photo_url", "address_display", "beds", "baths", 
                               "sqft_display", "price_display", "listing_id", "property_id", 
                               "status", "saved_resource_id",
                               'full_address_display',
                               'address',
                               'city',
                               'state_code',
                               'postal_code',
                               'lat',
                               'lon',
                               'agent_id',
                               'property_type',
                               'days_on_realtor',
                               'baths_half',
                               'baths_full',
                               'neighborhood_highlight',
                               'price_for_highlight',
                               'age_on_market_or_realtor',
                               'search_flags.is_price_reduced',
                               'search_flags.is_new_listing',
                               'search_flags.is_pending',
                               'search_flags.is_contingent'
                               ),
               nameActual = c("pricePerSF", "priceListingMedian", "priceRentMedian",
                              "countForSale",
                              "countNewListings",
                              "countOpenHouse",
                              "countSoldRecently",
                              "countForeclosures",
                              "countPriceReduced",
                              "slugLDP", "urlPhoto", "addressDisplay", "countBeds", "countBaths", 
                              "areaSF", "priceListing", "idListing", "idProperty", 
                              "statusListing", "idResouceSaved",
                              'addressDisplayFull',
                              'addressProperty',
                              'cityProperty',
                              'stateProperty',
                              'zipcodeProperty',
                              'latitudeProperty',
                              'longitudeProperty',
                              'idAgent',
                              'typeProperty',
                              'countDaysOnRealtor',
                              'countBathsHalf',
                              'countBathsFull',
                              'slugNeighborhoodHighlight',
                              'priceHighlight',
                              'remove',
                              'isPriceReduced',
                              'isListingNew',
                              'isPendingSale',
                              'isContingent'
               ))
  }


# rates -------------------------------------------------------------------

# https://www.realtor.com/mrtg_handler/get_trends_data

#' Gets current mortgage rates
#'
#' @param return_wide if \code{TRUE} widens data and removes duration and benchmark variables
#'
#' @return
#' @export
#'
#' @examples
#' get_mortage_rates(return_wide = F)
get_mortage_rates <-
  function(return_wide = F) {
    data <- 
      "https://www.realtor.com/mrtg_handler/get_trends_data" %>% 
      jsonlite::fromJSON(flatten = T, simplifyDataFrame = T) %>% 
      .$rate_trends %>% 
      dplyr::as_data_frame()
    
    df_types <- 
      data_frame(
      typeRate = c(
        'pct30YearFixed',
        'pct15YearFixed',
        'pct5OneARM',
        'pct20YearFixed',
        'pct30YeaFixedFHA',
        'pct30YearFixedFHA',
        'pct10YearFixed',
        'pct7OneARM'
      ),
      durationLoanMonths = c(360,
                             15 * 12,
                             5 * 12,
                             20 * 12,
                             360,
                             360,
                             120,
                             84),
      typeBenchmark = c(
        'fixed',
        'fixed',
        'floating',
        'fixed',
        'fixed',
        'fixed',
        'fixed',
        'floating'
      )
    )
    
    data <-
      data %>%
      purrr::set_names(
        c(
          "year",
          "month",
          "date",
          'pct30YearFixed',
          'pct15YearFixed',
          'pct5OneARM',
          'pct20YearFixed',
          'pct30YeaFixedFHA',
          'pct30YearFixedFHA',
          'pct10YearFixed',
          'pct7OneARM'
        )
      ) %>%
      tidyr::unite(dateData, year, month, date, sep = "-") %>%
      mutate(dateData = dateData %>% lubridate::ymd()) %>%
      gather(typeRate, value, -dateData) %>%
      mutate(value = value / 100) %>% 
      arrange(dateData)
    
    data <- 
      data %>%
      left_join(df_types) %>%
      select(dateData, typeRate, durationLoanMonths, typeBenchmark, value) %>% 
      suppressMessages()
    
    if (return_wide) {
    data <- 
      data %>% 
        select(-one_of(c("durationLoanMonths", "typeBenchmark"))) %>% 
        spread(typeRate, value)
    }
    data
  }



# market data -------------------------------------------------------------

# https://www.realtor.com/median_prices?city=Bethesda&state_code=MD

parse_location <- 
  function(location_name = c("Bethesda, MD")) {
    if (!location_name %>% str_detect("\\,")){
      stop("I don't detect a comma buddy, we need a city seperated by a location, try again!!")
    }
    
    location_slugs <- location_name %>% str_split("\\,") %>% flatten_chr() %>% str_trim()
    
    data_frame(nameCity = location_slugs[[1]], slugState = location_slugs[[2]])
    
  }

.generate_market_url <- 
  function(location_name = c("Bethesda, MD")) {
    df_loc_slug <- 
      location_name %>% 
      str_trim() %>% 
      parse_location()
    city <- 
      df_loc_slug$nameCity
    
    city_slug <- 
      city %>% URLencode()
    state <- df_loc_slug$slugState
    
    if (state %>% nchar() > 4) {
      stop("Has to be the 2 digit state slug buddy")
    }
    
    url <- 
      glue::glue('https://www.realtor.com/median_prices?city={city_slug}&state_code={state}') %>% 
      as.character()
    
    df_loc_slug %>% 
      mutate(nameLocationSearch = location_name) %>% 
      mutate(urlAPIRealtor = url) %>% 
      select(nameLocationSearch, everything())
    
  }

.generate_market_urls <- 
  function(location_names = c("Bethesda, MD")) {
    .generate_market_url_safe <- 
      purrr::possibly(.generate_market_url, data_frame())
    location_names %>% 
      map_df(function(location_name){
        .generate_market_url_safe(location_name = location_name)
      })
  }

.parse_market_data_url <- 
  function(url = "https://www.realtor.com/median_prices?city=Bethesda&state_code=MD") {
    data <- 
      url %>% jsonlite::fromJSON(simplifyVector = T, simplifyDataFrame = T, flatten = T)
    data <- 
      data$data %>% 
      flatten_df() %>% 
      dplyr::select(-matches("_display"))
    
    df_names <- dictionary_realtor_names()
    actual_names <- 
      names(data) %>% 
      map_chr(function(name){
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>% 
            message()
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    data %>% 
      purrr::set_names(actual_names) %>% 
      mutate(urlAPIRealtor = url)
    
  }


.parse_market_data_urls <- 
  function(urls = "https://www.realtor.com/median_prices?city=Bethesda&state_code=MD", 
           return_message = TRUE) {
    df <-
      data_frame()
    
    success <- function(res) {
      url <-
        res$url
      
      if (return_message) {
        glue::glue("Parsing {url}") %>%
          message()
      }
      .parse_market_data_url_safe <-
        purrr::possibly(.parse_market_data_url , data_frame())
      
      all_data <-
        .parse_market_data_url_safe(url = url)
      
      
      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

#' Get median market statistics for specified locations
#' 
#' Returns summary market information for the specified
#' location.  The locaiton name must be a city bounded by a comma
#'
#' @param location_names vector of location names, location name must contain
#' a city name and a comma ie "Brooklyn, NY"
#' 
#' @param return_message if \code{TRUE} returns a message
#' @param ... 
#'
#' @return a \code{data_frame}
#' @export
#'
#' @examples
#' get_realtor_locations_median_prices(location_names = "Greenwich, CT")
get_realtor_locations_median_prices <- 
  function(location_names = NULL,
           return_message = TRUE,
           ...) {
    
    if (location_names %>% purrr::is_null()) {
      stop("Please enter location names!!")
    }
    
    .generate_market_urls_safe <-
      purrr::possibly(.generate_market_urls, data_frame())
    
    df_urls <- 
      .generate_market_urls_safe(location_names = location_names)
    
    if (df_urls %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    
    .parse_market_data_urls_safe <- 
      purrr::possibly(.parse_market_data_urls, data_frame())
    
    all_data <-
      .parse_market_data_urls_safe(urls = df_urls$urlAPIRealtor, return_message = return_message)
    
    if (all_data %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    
    
    if (all_data %>% tibble::has_name("priceRentMedian")) {
      all_data <-
        all_data %>%
        mutate(pctRentYield = (priceRentMedian * 12) / priceListingMedian)
    }
    
    if (all_data %>% tibble::has_name("priceListingMedian")) {
      all_data <-
        all_data %>%
        mutate(areaSFMedian = (priceListingMedian / pricePerSF) %>% round(digits = 0))
    }
    
    all_data <-
      all_data %>%
      left_join(df_urls) %>%
      select(one_of(
        c(
          'nameLocationSearch',
          'nameCity',
          'slugState',
          'pricePerSF',
          'priceRentMedian',
          'pctRentYield',
          'areaSFMedian'
        )
      ),
      everything()) %>%
      suppressMessages() %>%
      suppressWarnings() %>% 
      asbmisc::remove_na_columns()
    all_data
  }


# market_vitality ---------------------------------------------------------

.parse_market_vitality_url <- 
  function(url = "https://www.realtor.com/home_page/vitality?location=Bethesda%2C+MD") {
    data <- 
      url %>% 
      jsonlite::fromJSON(flatten = T, simplifyDataFrame = T)
    
    df_names <-dictionary_realtor_names()
    
    data_listings <- data$new_listings %>% as_data_frame()
    
    actual_names <- 
      names(data_listings) %>% 
      map_chr(function(name){
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>% 
            message()
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    data_listings <- 
      data_listings %>%
      purrr::set_names(actual_names) %>%
      mutate(
        priceListing = priceListing %>% readr::parse_number(),
        areaSF = areaSF %>% readr::parse_number(),
        urlListing = glue::glue("https://www.realtor.com/{slugLDP}") %>% as.character()
      ) %>%
      select(idListing, addressDisplay, statusListing, 
             everything()) %>% 
      remove_na() %>% 
      select(-one_of("slugLDP")) %>% 
      suppressMessages()
  
    data <-
      data[c(
        "for_sale_count",
        "new_listing_count",
        "open_house_count",
        "recently_sold_count",
        "foreclosure_count",
        "price_reduced_count"
      )] %>%
      as_data_frame()
    
    actual_names <- 
      names(data) %>% 
      map_chr(function(name){
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>% 
            message()
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    data <-
      data %>%
      purrr::set_names(actual_names) %>%
      mutate(urlAPIRealtor = url,
             dataListingsRecent = list(data_listings))
    
    data
  }

.parse_market_vitality_urls <- 
  function(urls =  "https://www.realtor.com/home_page/vitality?location=Bethesda%2C+MD", 
           return_message = T) {
    df <-
      data_frame()
    
    success <- function(res) {
      url <-
        res$url
      
      if (return_message) {
        glue::glue("Parsing {url}") %>%
          message()
      }
      .parse_market_vitality_url_safe <-
        purrr::possibly(.parse_market_vitality_url , data_frame())
      
      all_data <-
        .parse_market_vitality_url_safe(url = url)
      
      
      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      data_frame()
    }
    urls %>%
      map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df    
  }


.generate_market_vitality_url <- 
  function(location_name = c("Bethesda, MD")) {
    df_loc_slug <- 
      location_name %>% 
      str_trim() %>% 
      parse_location()
    city <- 
      df_loc_slug$nameCity
    
    location_slug <- 
      location_name %>% 
      str_replace_all("\\,", "") %>% 
      URLencode()
    state <- df_loc_slug$slugState
    
    if (state %>% nchar() > 4) {
      stop("Has to be the 2 digit state slug buddy")
    }
    
    url <- 
      glue::glue('https://www.realtor.com/home_page/vitality?location={location_slug}') %>% 
      as.character()
    
    df_loc_slug %>% 
      mutate(nameLocationSearch = location_name) %>% 
      mutate(urlAPIRealtor = url) %>% 
      select(nameLocationSearch, everything())
    
  }

.generate_market_vitality_urls <- 
  function(location_names = c("Bethesda, MD")) {
    .generate_market_vitality_url_safe <- 
      purrr::possibly(.generate_market_vitality_url, data_frame())
    
    location_names %>% 
      map_df(function(location_name){
        .generate_market_vitality_url_safe(location_name = location_name)
      })
  }


#' Meta level market data
#' 
#' Acquires meta level information
#' for specified markets
#'
#' @param location_names vector of location names, location name must contain
#' a city name and a comma ie "Brooklyn, NY"
#' 
#' @param return_message if \code{TRUE} returns a message
#' @param ... 
#'
#' @return a \code{data_frame}
#' @export
#'

#'
#' @return
#' @export
#'
#' @examples
#' get_realtor_locations_vitality(location_names = c("La Jolla, CA", "Manhattan, NY", "Bethany, DE"))
get_realtor_locations_vitality <- 
  function(location_names = NULL,
           return_message = TRUE,
           ...) {
    
    if (location_names %>% purrr::is_null()) {
      stop("Please enter location names!!")
    }
    
    .generate_market_vitality_urls_safef <-
      purrr::possibly(.generate_market_vitality_urls, data_frame())
    
    df_urls <- 
      .generate_market_vitality_urls_safef(location_names = location_names)
    
    if (df_urls %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    
    .parse_market_vitality_urls_safe <- 
      purrr::possibly(.parse_market_vitality_urls, data_frame())
    
    all_data <-
      .parse_market_vitality_urls_safe(urls = df_urls$urlAPIRealtor, return_message = return_message)
    
    if (all_data %>% nrow() == 0) {
      "No results" %>% message()
      return(invisible())
    }
    all_data <-
      all_data %>%
      left_join(df_urls) %>%
      select(nameLocationSearch,
             nameCity,
             slugState,
             everything()) %>%
      suppressMessages() %>%
      asbmisc::remove_na_columns()
    
    
    all_data
  }

# api ---------------------------------------------------------------------
## https://www.realtor.com/browse_modules/homes_near_street?price=239000&postal_code=20816&median_price=1132000&type=homes_near_street&vis_id=6b0d44ae-f4d6-41f0-8feb-e6491ab43fe9&mcm_id=03714656198478469204855792545062287725&address=5301+Westbard+Cir+Apt+323&user_id=&city=Bethesda&coordinates=38.964595%2C-77.109017


.parse_realtor_api_near_url <- 
  function(url = "https://www.realtor.com/browse_modules/homes_near_street?postal_code=20816vis_id=6b0d44ae-f4d6-41f0-8feb-e6491ab43fe9&mcm_id=03714656198478469204855792545062287725&city=New+York&coordinates=40.74516%2C-73.97852") {
    data <- 
      url %>% 
      jsonlite::fromJSON(simplifyVector = T, simplifyDataFrame = T, flatten = T)
    
    df_properties <-
      data$properties %>%
      as_data_frame()
    
    df_names <- dictionary_realtor_names()
    actual_names <-
      names(df_properties) %>%
      map_chr(function(name) {
        df_row <- df_names %>% filter(nameRealtor == name)
        if (df_row %>% nrow() == 0) {
          glue::glue("Missing {name}") %>%
            message()
          return(name)
        }
        df_row %>% pull(nameActual)
      })
    
    df_properties <-
      df_properties %>%
      purrr::set_names(actual_names) %>%
      dplyr::select(-matches("remove")) %>%
      suppressMessages()
    
    df_properties <- 
      df_properties %>% 
      .munge_realtor() %>% 
      mutate(urlAPIRealtor = url)
    
    df_properties
  }

# listings ----------------------------------------------------------------

.parse_listing_url <- 
  function(url = "https://www.realtor.com/realestateandhomes-detail/5301-Westbard-Cir-Apt-323_Bethesda_MD_20816_M63437-59115") {
   page <- 
     url %>% 
     read_html()
   
   broker_nodes <- 
     page %>% 
     html_nodes('#ldp-branding .font-bold') 
   
   broker_nodes %>% html_text()
   broker_nodes %>% html_structure()
   
   base_nodes <- 
     page %>% 
     html_nodes('#key-fact-carousel .ellipsis')
  }