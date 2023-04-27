CLASS zcl_mcbc_mdrnabap_meshes DEFINITION
  PUBLIC  INHERITING FROM zcl_mcbc_modernabap
  FINAL CREATE PRIVATE
  GLOBAL FRIENDS zcl_mcbc_modernabap.

  PUBLIC SECTION.

    METHODS:
      forward_inverse_association,
      reflexive_association .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MCBC_MDRNABAP_MESHES IMPLEMENTATION.


  METHOD forward_inverse_association.

    TYPES : tts_airline   TYPE SORTED TABLE OF scarr WITH UNIQUE KEY carrid,
            tts_alschdule TYPE SORTED TABLE OF spfli WITH UNIQUE KEY carrid connid,
            tts_alconn    TYPE SORTED TABLE OF sflight WITH UNIQUE KEY carrid connid fldate.

    TYPES : BEGIN OF MESH tymesh_airlines,
              airline   TYPE tts_airline      " AirLines
                  ASSOCIATION to_schedule TO alschdule
                           ON carrid = carrid USING KEY primary_key,

              alschdule TYPE tts_alschdule  " AirLines Schedule
                ASSOCIATION to_alconn TO alconn
                         ON carrid = carrid
                        AND connid = connid USING KEY primary_key,

              alconn    TYPE tts_alconn,       " AirLines Connection
            END OF MESH tymesh_airlines.

    DATA alinfo TYPE tymesh_airlines.

    SELECT *
      FROM scarr
      INTO TABLE @alinfo-airline.

    SELECT *
      FROM spfli
      INTO TABLE @alinfo-alschdule.

    SELECT *
      FROM sflight
      INTO TABLE @alinfo-alconn.

*   Forward Associations in Mesh Paths
    out->begin_section( 'Forward Associations in Mesh Paths'
      " To access a row in Meshes
      )->write( alinfo-airline[ carrid = 'AA' ]
      )->write( alinfo-airline\to_schedule[ alinfo-airline[ carrid = 'AA' ] connid = '0064' ]
      )->write( alinfo-airline\to_schedule[ alinfo-airline[ carrid = 'AA' ] connid = '0064'  ]\to_alconn[ fldate = '20210820' ] ).

    " To access set of rows in Meshes (LOOP AT; FOR ..IN)
    out->write( VALUE tts_alschdule( FOR <ls_sch> IN alinfo-airline\to_schedule[ alinfo-airline[ carrid = 'AA' ] ] ( <ls_sch> ) )
      )->write( VALUE tts_alconn( FOR <ls_conn> IN alinfo-airline\to_schedule[ alinfo-airline[ carrid = 'AA' ] ]\to_alconn[ ] ( <ls_conn> ) ) ).

    " To access set of rows in Meshes with where condition
    out->write( VALUE tts_alconn( FOR <ls_conn> IN alinfo-airline\to_schedule[ alinfo-airline[ carrid = 'AA' ] ]\to_alconn[ WHERE fldate BETWEEN '20210101' AND '20210331' ] ( <ls_conn> ) ) ).

    out->next_section( 'Inverse Associations in Mesh Paths'
      )->write( alinfo-alconn[ carrid = 'AA' connid = '0064' fldate = '20210820' ] ).

*   Inverse Associations in Mesh Paths
    out->write( alinfo-alconn\^to_alconn~alschdule[ alinfo-alconn[ carrid = 'AA' connid = '0064' fldate = '20210820' ] ]
      )->write( alinfo-alconn\^to_alconn~alschdule[ alinfo-alconn[ carrid = 'AA' connid = '0064' fldate = '20210820' ] ]\^to_schedule~airline[ ] ).

  ENDMETHOD.


  METHOD reflexive_association.

    TYPES: BEGIN OF ty_place,
             id        TYPE c LENGTH 3,
             parent_id TYPE c LENGTH 3,
             name      TYPE string,
             ord_id    TYPE i,
           END OF ty_place,

           tt_place  TYPE STANDARD TABLE OF ty_place WITH EMPTY KEY,
           tts_place TYPE SORTED TABLE OF ty_place WITH UNIQUE KEY id
                     WITH NON-UNIQUE SORTED KEY by_parent COMPONENTS parent_id
                     WITH NON-UNIQUE SORTED KEY by_ord_id COMPONENTS ord_id,
           BEGIN OF MESH tm_place,
             places TYPE tts_place
                 ASSOCIATION _places TO places
                          ON parent_id = id,
           END OF MESH tm_place.

    DATA lm_place TYPE tm_place.
    DATA lt_ord_places TYPE tt_place.

    lm_place-places = VALUE tts_place( ( id = 'IN' parent_id = '' ord_id = 1 name = 'IN-India' )
                                       ( id = 'TN' parent_id = 'IN' ord_id = 2 name = 'IN-TN-Tamilnadu' )
                                       ( id = 'KA' parent_id = 'IN' ord_id = 3 name = 'IN-KA-Karnataka' )
                                       ( id = 'CHE' parent_id = 'TN' ord_id = 4 name = 'IN-TN-CHE-Chennai' )
                                       ( id = 'CBE' parent_id = 'TN' ord_id = 5 name = 'IN-TN-CBE-Coimbatore' )
                                       ( id = 'KVP' parent_id = 'CBE' ord_id = 6 name = 'IN-TN-CBE-KVP-Kovaipudur' )
                                       ( id = 'SVP' parent_id = 'CBE' ord_id = 7 name = 'IN-TN-CBE-SVP-Saravanampatti' )
                                       ( id = 'SA' parent_id = 'TN' ord_id = 8 name = 'IN-TN-SA-Salem' )
                                       ( id = 'BLR' parent_id = 'KA' ord_id = 9 name = 'IN-KA-BLR-Bengaluru' )
                                       ( id = 'MYS' parent_id = 'KA' ord_id = 10 name = 'IN-KA-MYS-Mysore  ' )
                                       ( id = 'EU' parent_id = '' ord_id = 101 name = 'EU-Europe' )
                                       ( id = 'DE' parent_id = 'EU' ord_id = 102 name = 'EU-DE-Germany' )
                                       ( id = 'ST' parent_id = 'DE' ord_id = 103 name = 'EU-DE-ST-Stuttgart' )
                                       ( id = 'FE' parent_id = 'ST' ord_id = 104 name = 'EU-DE-ST-FE-Feuerbach' )
                                       ( id = 'RE' parent_id = 'ST' ord_id = 105 name = 'EU-DE-ST-RE-Renningen' )
                                       ( id = 'WE' parent_id = 'ST' ord_id = 106 name = 'EU-DE-ST-WE-Weilimdorf' )
                                       ( id = 'MUC' parent_id = 'DE' ord_id = 107 name = 'EU-DE-MUC-Munich' )
                                       ( id = 'GB' parent_id = 'MUC' ord_id = 108 name = 'EU-DE-MUC-GB-Grasburnn' )
                                       ( id = 'OB' parent_id = 'MUC' ord_id = 109 name = 'EU-DE-MUC-OB-Ottoburnn' )
                                       ( id = 'BL' parent_id = 'DE' ord_id = 110 name = 'EU-DE-BL-Berlin' ) ).

    DATA(id) = 'TN '.
    cl_demo_input=>request( CHANGING field = id ).
    id = to_upper( id ).

    out->begin_section( 'Place' ).

    lt_ord_places = VALUE #( FOR place IN lm_place-places USING KEY by_ord_id ( place ) ).
*      LOOP AT lt_places-place INTO DATA(ls_place) USING KEY by_ord_id.
*      APPEND ls_place TO lt_ord_places.
*      ENDLOOP.
    out->write( lt_ord_places ).

    IF line_exists( lm_place-places[ id = id ] ).
      out->next_section( '\_places (immediate children)' ).
      out->write( VALUE tt_place(
         FOR <node> IN lm_place-places\_places[ lm_place-places[ id = id ] ]
            ( <node> ) ) ).

      out->next_section( '\_places+ (immediate and all children in hierarchy)' ).
      out->write( VALUE tt_place(
         FOR <node> IN lm_place-places\_places+[ lm_place-places[ id = id ] ]
            ( <node> ) ) ).

      out->next_section( '\_places* (Parent node, immediate and all children in hierarchy)' ).
      out->write( VALUE tt_place(
         FOR <node> IN lm_place-places\_places*[ lm_place-places[ id = id ] ]
            ( <node> ) ) ) ##PRIMKEY[BY_PARENT].
    ELSE.
      out->write( `Enter a valid ID ...` ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
