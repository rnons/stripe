{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Currency where

import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.Ratio                     ((%))
import           Numeric                        (fromRat, showFFloat)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toBytestring)

------------------------------------------------------------------------------
-- | Stripe supports 138 currencies
data Currency =
    AED -- ^  United Arab Emirates Dirham
  | AFN -- ^  Afghan Afghani
  | ALL -- ^  Albanian Lek
  | AMD -- ^  Armenian Dram
  | ANG -- ^  Netherlands Antillean Gulden
  | AOA -- ^  Angolan Kwanza
  | ARS -- ^  Argentine Peso
  | AUD -- ^  Australian Dollar
  | AWG -- ^  Aruban Florin
  | AZN -- ^  Azerbaijani Manat
  | BAM -- ^  Bosnia & Herzegovina Convertible Mark
  | BBD -- ^  Barbadian Dollar
  | BDT -- ^  Bangladeshi Taka
  | BGN -- ^  Bulgarian Lev
  | BIF -- ^  Burundian Franc
  | BMD -- ^  Bermudian Dollar
  | BND -- ^  Brunei Dollar
  | BOB -- ^  Bolivian Boliviano
  | BRL -- ^  Brazilian Real
  | BSD -- ^  Bahamian Dollar
  | BWP -- ^  Botswana Pula
  | BZD -- ^  Belize Dollar
  | CAD -- ^  Canadian Dollar
  | CDF -- ^  Congolese Franc
  | CHF -- ^  Swiss Franc
  | CLP -- ^  Chilean Peso
  | CNY -- ^  Chinese Renminbi Yuan
  | COP -- ^  Colombian Peso
  | CRC -- ^  Costa Rican Colón
  | CVE -- ^  Cape Verdean Escudo
  | CZK -- ^  Czech Koruna
  | DJF -- ^  Djiboutian Franc
  | DKK -- ^  Danish Krone
  | DOP -- ^  Dominican Peso
  | DZD -- ^  Algerian Dinar
  | EEK -- ^  Estonian Kroon
  | EGP -- ^  Egyptian Pound
  | ETB -- ^  Ethiopian Birr
  | EUR -- ^  Euro
  | FJD -- ^  Fijian Dollar
  | FKP -- ^  Falkland Islands Pound
  | GBP -- ^  British Pound
  | GEL -- ^  Georgian Lari
  | GIP -- ^  Gibraltar Pound
  | GMD -- ^  Gambian Dalasi
  | GNF -- ^  Guinean Franc
  | GTQ -- ^  Guatemalan Quetzal
  | GYD -- ^  Guyanese Dollar
  | HKD -- ^  Hong Kong Dollar
  | HNL -- ^  Honduran Lempira
  | HRK -- ^  Croatian Kuna
  | HTG -- ^  Haitian Gourde
  | HUF -- ^  Hungarian Forint
  | IDR -- ^  Indonesian Rupiah
  | ILS -- ^  Israeli New Sheqel
  | INR -- ^  Indian Rupee
  | ISK -- ^  Icelandic Króna
  | JMD -- ^  Jamaican Dollar
  | JPY -- ^  Japanese Yen
  | KES -- ^  Kenyan Shilling
  | KGS -- ^  Kyrgyzstani Som
  | KHR -- ^  Cambodian Riel
  | KMF -- ^  Comorian Franc
  | KRW -- ^  South Korean Won
  | KYD -- ^  Cayman Islands Dollar
  | KZT -- ^  Kazakhstani Tenge
  | LAK -- ^  Lao Kip
  | LBP -- ^  Lebanese Pound
  | LKR -- ^  Sri Lankan Rupee
  | LRD -- ^  Liberian Dollar
  | LSL -- ^  Lesotho Loti
  | LTL -- ^  Lithuanian Litas
  | LVL -- ^  Latvian Lats
  | MAD -- ^  Moroccan Dirham
  | MDL -- ^  Moldovan Leu
  | MGA -- ^  Malagasy Ariary
  | MKD -- ^  Macedonian Denar
  | MNT -- ^  Mongolian Tögrög
  | MOP -- ^  Macanese Pataca
  | MRO -- ^  Mauritanian Ouguiya
  | MUR -- ^  Mauritian Rupee
  | MVR -- ^  Maldivian Rufiyaa
  | MWK -- ^  Malawian Kwacha
  | MXN -- ^  Mexican Peso
  | MYR -- ^  Malaysian Ringgit
  | MZN -- ^  Mozambican Metical
  | NAD -- ^  Namibian Dollar
  | NGN -- ^  Nigerian Naira
  | NIO -- ^  Nicaraguan Córdoba
  | NOK -- ^  Norwegian Krone
  | NPR -- ^  Nepalese Rupee
  | NZD -- ^  New Zealand Dollar
  | PAB -- ^  Panamanian Balboa
  | PEN -- ^  Peruvian Nuevo Sol
  | PGK -- ^  Papua New Guinean Kina
  | PHP -- ^  Philippine Peso
  | PKR -- ^  Pakistani Rupee
  | PLN -- ^  Polish Złoty
  | PYG -- ^  Paraguayan Guaraní
  | QAR -- ^  Qatari Riyal
  | RON -- ^  Romanian Leu
  | RSD -- ^  Serbian Dinar
  | RUB -- ^  Russian Ruble
  | RWF -- ^  Rwandan Franc
  | SAR -- ^  Saudi Riyal
  | SBD -- ^  Solomon Islands Dollar
  | SCR -- ^  Seychellois Rupee
  | SEK -- ^  Swedish Krona
  | SGD -- ^  Singapore Dollar
  | SHP -- ^  Saint Helenian Pound
  | SLL -- ^  Sierra Leonean Leone
  | SOS -- ^  Somali Shilling
  | SRD -- ^  Surinamese Dollar
  | STD -- ^  São Tomé and Príncipe Dobra
  | SVC -- ^  Salvadoran Colón
  | SZL -- ^  Swazi Lilangeni
  | THB -- ^  Thai Baht
  | TJS -- ^  Tajikistani Somoni
  | TOP -- ^  Tongan Paʻanga
  | TRY -- ^  Turkish Lira
  | TTD -- ^  Trinidad and Tobago Dollar
  | TWD -- ^  New Taiwan Dollar
  | TZS -- ^  Tanzanian Shilling
  | UAH -- ^  Ukrainian Hryvnia
  | UGX -- ^  Ugandan Shilling
  | USD -- ^  United States Dollar
  | UYU -- ^  Uruguayan Peso
  | UZS -- ^  Uzbekistani Som
  | VND -- ^  Vietnamese Đồng
  | VUV -- ^  Vanuatu Vatu
  | WST -- ^  Samoan Tala
  | XAF -- ^  Central African Cfa Franc
  | XCD -- ^  East Caribbean Dollar
  | XOF -- ^  West African Cfa Franc
  | XPF -- ^  Cfp Franc
  | YER -- ^  Yemeni Rial
  | ZAR -- ^  South African Rand
  | ZMW -- ^  Zambian Kwacha
  | UnknownCurrency -- ^  Unknown Currency
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Currency` JSON instances
instance FromJSON Currency where
   parseJSON (String "aed") = pure AED
   parseJSON (String "afn") = pure AFN
   parseJSON (String "all") = pure ALL
   parseJSON (String "amd") = pure AMD
   parseJSON (String "ang") = pure ANG
   parseJSON (String "aoa") = pure AOA
   parseJSON (String "ars") = pure ARS
   parseJSON (String "aud") = pure AUD
   parseJSON (String "awg") = pure AWG
   parseJSON (String "azn") = pure AZN
   parseJSON (String "bam") = pure BAM
   parseJSON (String "bbd") = pure BBD
   parseJSON (String "bdt") = pure BDT
   parseJSON (String "bgn") = pure BGN
   parseJSON (String "bif") = pure BIF
   parseJSON (String "bmd") = pure BMD
   parseJSON (String "bnd") = pure BND
   parseJSON (String "bob") = pure BOB
   parseJSON (String "brl") = pure BRL
   parseJSON (String "bsd") = pure BSD
   parseJSON (String "bwp") = pure BWP
   parseJSON (String "bzd") = pure BZD
   parseJSON (String "cad") = pure CAD
   parseJSON (String "cdf") = pure CDF
   parseJSON (String "chf") = pure CHF
   parseJSON (String "clp") = pure CLP
   parseJSON (String "cny") = pure CNY
   parseJSON (String "cop") = pure COP
   parseJSON (String "crc") = pure CRC
   parseJSON (String "cve") = pure CVE
   parseJSON (String "czk") = pure CZK
   parseJSON (String "djf") = pure DJF
   parseJSON (String "dkk") = pure DKK
   parseJSON (String "dop") = pure DOP
   parseJSON (String "dzd") = pure DZD
   parseJSON (String "eek") = pure EEK
   parseJSON (String "egp") = pure EGP
   parseJSON (String "etb") = pure ETB
   parseJSON (String "eur") = pure EUR
   parseJSON (String "fjd") = pure FJD
   parseJSON (String "fkp") = pure FKP
   parseJSON (String "gbp") = pure GBP
   parseJSON (String "gel") = pure GEL
   parseJSON (String "gip") = pure GIP
   parseJSON (String "gmd") = pure GMD
   parseJSON (String "gnf") = pure GNF
   parseJSON (String "gtq") = pure GTQ
   parseJSON (String "gyd") = pure GYD
   parseJSON (String "hkd") = pure HKD
   parseJSON (String "hnl") = pure HNL
   parseJSON (String "hrk") = pure HRK
   parseJSON (String "htg") = pure HTG
   parseJSON (String "huf") = pure HUF
   parseJSON (String "idr") = pure IDR
   parseJSON (String "ils") = pure ILS
   parseJSON (String "inr") = pure INR
   parseJSON (String "isk") = pure ISK
   parseJSON (String "jmd") = pure JMD
   parseJSON (String "jpy") = pure JPY
   parseJSON (String "kes") = pure KES
   parseJSON (String "kgs") = pure KGS
   parseJSON (String "khr") = pure KHR
   parseJSON (String "kmf") = pure KMF
   parseJSON (String "krw") = pure KRW
   parseJSON (String "kyd") = pure KYD
   parseJSON (String "kzt") = pure KZT
   parseJSON (String "lak") = pure LAK
   parseJSON (String "lbp") = pure LBP
   parseJSON (String "lkr") = pure LKR
   parseJSON (String "lrd") = pure LRD
   parseJSON (String "lsl") = pure LSL
   parseJSON (String "ltl") = pure LTL
   parseJSON (String "lvl") = pure LVL
   parseJSON (String "mad") = pure MAD
   parseJSON (String "mdl") = pure MDL
   parseJSON (String "mga") = pure MGA
   parseJSON (String "mkd") = pure MKD
   parseJSON (String "mnt") = pure MNT
   parseJSON (String "mop") = pure MOP
   parseJSON (String "mro") = pure MRO
   parseJSON (String "mur") = pure MUR
   parseJSON (String "mvr") = pure MVR
   parseJSON (String "mwk") = pure MWK
   parseJSON (String "mxn") = pure MXN
   parseJSON (String "myr") = pure MYR
   parseJSON (String "mzn") = pure MZN
   parseJSON (String "nad") = pure NAD
   parseJSON (String "ngn") = pure NGN
   parseJSON (String "nio") = pure NIO
   parseJSON (String "nok") = pure NOK
   parseJSON (String "npr") = pure NPR
   parseJSON (String "nzd") = pure NZD
   parseJSON (String "pab") = pure PAB
   parseJSON (String "pen") = pure PEN
   parseJSON (String "pgk") = pure PGK
   parseJSON (String "php") = pure PHP
   parseJSON (String "pkr") = pure PKR
   parseJSON (String "pln") = pure PLN
   parseJSON (String "pyg") = pure PYG
   parseJSON (String "qar") = pure QAR
   parseJSON (String "ron") = pure RON
   parseJSON (String "rsd") = pure RSD
   parseJSON (String "rub") = pure RUB
   parseJSON (String "rwf") = pure RWF
   parseJSON (String "sar") = pure SAR
   parseJSON (String "sbd") = pure SBD
   parseJSON (String "scr") = pure SCR
   parseJSON (String "sek") = pure SEK
   parseJSON (String "sgd") = pure SGD
   parseJSON (String "shp") = pure SHP
   parseJSON (String "sll") = pure SLL
   parseJSON (String "sos") = pure SOS
   parseJSON (String "srd") = pure SRD
   parseJSON (String "std") = pure STD
   parseJSON (String "svc") = pure SVC
   parseJSON (String "szl") = pure SZL
   parseJSON (String "thb") = pure THB
   parseJSON (String "tjs") = pure TJS
   parseJSON (String "top") = pure TOP
   parseJSON (String "try") = pure TRY
   parseJSON (String "ttd") = pure TTD
   parseJSON (String "twd") = pure TWD
   parseJSON (String "tzs") = pure TZS
   parseJSON (String "uah") = pure UAH
   parseJSON (String "ugx") = pure UGX
   parseJSON (String "usd") = pure USD
   parseJSON (String "uyu") = pure UYU
   parseJSON (String "uzs") = pure UZS
   parseJSON (String "vnd") = pure VND
   parseJSON (String "vuv") = pure VUV
   parseJSON (String "wst") = pure WST
   parseJSON (String "xaf") = pure XAF
   parseJSON (String "xcd") = pure XCD
   parseJSON (String "xof") = pure XOF
   parseJSON (String "xpf") = pure XPF
   parseJSON (String "yer") = pure YER
   parseJSON (String "zar") = pure ZAR
   parseJSON (String "zmw") = pure ZMW
   parseJSON _              = pure UnknownCurrency

instance ToStripeParam Currency where
  toStripeParam currency =
    (("currency", toBytestring currency) :)

------------------------------------------------------------------------------
-- | Show an amount accounting for zero currencies
--
-- https:\/\/support.stripe.com\/questions\/which-zero-decimal-currencies-does-stripe-support
showAmount
  :: Currency -- ^ `Currency`
  -> Int      -- ^ `Amount`
  -> String
showAmount cur amt =
  case cur of
   USD -> "$" ++ show2places (currencyDivisor cur amt)
   _   -> show2places (currencyDivisor cur amt) ++ " " ++ show cur
  where
    show2places v = showFFloat (Just 2) v ""

------------------------------------------------------------------------------
-- currency division funtion accounting for zero currencies
--
-- https:\/\/support.stripe.com\/questions\/which-zero-decimal-currencies-does-stripe-support
currencyDivisor
    :: Currency -- ^ `Currency`
    -> (Int -> Float) -- ^ function to convert amount to a float
currencyDivisor cur =
  case cur of
    BIF -> zeroCurrency
    CLP -> zeroCurrency
    DJF -> zeroCurrency
    GNF -> zeroCurrency
    JPY -> zeroCurrency
    KMF -> zeroCurrency
    KRW -> zeroCurrency
    MGA -> zeroCurrency
    PYG -> zeroCurrency
    RWF -> zeroCurrency
    VND -> zeroCurrency
    VUV -> zeroCurrency
    XAF -> zeroCurrency
    XOF -> zeroCurrency
    XPF -> zeroCurrency
    EUR -> hundred
    USD -> hundred
    _   -> error $ "please submit a patch to currencyDivisor for this currency: " ++ show cur
  where
    zeroCurrency = fromIntegral
    hundred v    = fromRat $ (fromIntegral v) % (100 :: Integer)
