SELECT *
   FROM
     JSON_TABLE(
       '[ {"c1": null} ]',
       '$[*]' COLUMNS( c1 INT PATH '$.c1' ERROR ON ERROR )
     ) as jt;
