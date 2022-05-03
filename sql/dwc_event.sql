
SELECT

-- Metadata terms:

  'Event'                                              AS type
  ,'en'                                                AS language
  ,'http://creativecommons.org/publicdomain/zero/1.0/' AS license
  ,'RATO'                                              AS rightsHolder
  ,'http://www.inbo.be/en/norms-for-data-use'          AS accessRights
  ,NULL                                                AS datasetID
  ,'RATO'                                              AS institutionCode
  ,'RATO - daily operations commissioned by the province East Flanders, Belgium)'    AS datasetName
  ,'humanObservation'                    AS basisOfRecord

-- Darwin Core Event terms:

  ,p.'Dossier_ID'                                      AS eventID
  ,'RATO'                                              AS recordedBy
  ,CASE
    WHEN p.'Waarneming' LIKE '%Vastgesteld (in mÂ²)%' THEN
        SUBSTRING(p.'Waarneming',charindex('=', p.'Waarneming')+2)

 --     substr(trim(p.'waarneming_quantity'), 1, length(trim(p.'waarneming_quantity'))-1)
    WHEN p.'Waarneming' LIKE '%Vastgesteld (aantal)%' AND p.'waarneming_quantity' != '0; '
                                                      THEN
      substr(trim(p.'waarneming_quantity'), 1, length(trim(p.'waarneming_quantity'))-1)
    ELSE NULL
    END                                               AS organismQuantity
  ,CASE
    WHEN p.'Waarneming' LIKE '%Vastgesteld (in mÂ²)%' THEN 'square meter(s)'
    WHEN p.'Waarneming' LIKE '%Vastgesteld (aantal)%' AND p.'waarneming_quantity' != '0; ' THEN 'individual(s)'
    ELSE NULL
    END                                               AS organismQuantityType
  ,CASE
    -- (...) waarneming onzeker (...) cases:
    WHEN p.'Waarneming' LIKE '%onzeker%; '  THEN 'presence uncertain'


    -- Haard vastgesteld (...) cases:
    WHEN p.'Waarneming' LIKE 'Haard vastgesteld%'  OR

   -- Nest vastgesteld (...) cases:
      p.'Waarneming' LIKE 'Nest vastgesteld%'  OR

   -- Vastgesteld cases (...):
      p.'Waarneming' LIKE 'Vastgesteld%'  OR

   -- Lege cellen:
      p.'Waarneming' = ''        THEN 'present'


    ELSE 'absent'

    END                                              AS occurrenceStatus

--    AS samplingProtocol,
--    AS samplingEffort,




--    AS eventDate,


  , substr(p.'Aangemaakt_Datum',1,4) as eventDate
  ,*




FROM(

  SELECT
    SUBSTRING(input.'Waarneming',1,charindex('=', input.'Waarneming')-2) AS waarneming_type
    ,SUBSTRING(input.'Waarneming',charindex('=', input.'Waarneming')+2) AS waarneming_quantity
    ,*

  FROM input_data as input
  WHERE Domein != 'Werken'

  ) as p      /* p for processed data */






/*






  interim.waarneming_quantity, charindex(';' interim.waarneming_quantity) - 1) AS waarneming

FROM (
    SELECT

    FROM input_data as input )











    AS countryCode,
    AS municipality,
    AS verbatimLatitude,
    AS verbatimLongitude,
    AS
    AS verbatimCoordinateSystem,
    AS verbatimSRS,
    AS decimalLatitude,
    AS decimalLongitude,
    AS geodeticDatum,
    AS coordinateUncertainty,
    AS identifiedBy,
    AS scientificName,
    AS kingdom,
    AS taxonRank

*/




