assert(wlist(lugar)).
assert(witem(lugar,america)).
assert(witem(lugar,mexico)).
assert(wpar(america,mexico)).
assert(wlist(economia)).
assert(witem(economia,sector_primario)).
assert(witem(economia,agricultura)).
assert(wpar(sector_primario,agricultura)).
assert(witem(economia,ganaderia)).
assert(wpar(sector_primario,ganaderia)).
assert(witem(economia,arroz)).
assert(wpar(agricultura,arroz)).
assert(witem(economia,platano)).
assert(wpar(agricultura,platano)).
assert(witem(economia,cacao)).
assert(wpar(agricultura,cacao)).
assert(witem(economia,vacuna)).
assert(wpar(ganaderia,vacuna)).
assert(witem(lugar,norteamerica)).
assert(wpar(america,norteamerica)).
assert(witem(lugar,centroamerica)).
assert(wpar(america,centroamerica)).
assert(witem(lugar,sudamerica)).
assert(wpar(america,sudamerica)).
retract(wpar(america,mexico)).
assert(wpar(norteamerica,mexico)).
assert(witem(lugar,nicaragua)).
assert(wpar(centroamerica,nicaragua)).
assert(witem(lugar,guatemala)).
assert(wpar(centroamerica,guatemala)).
assert(witem(lugar,honduras)).
assert(wpar(centroamerica,honduras)).
assert(witem(lugar,panama)).
assert(wpar(centroamerica,panama)).
assert(witem(lugar,colombia)).
assert(wpar(sudamerica,colombia)).
assert(witem(lugar,venezuela)).
assert(wpar(sudamerica,venezuela)).
assert(witem(lugar,guayana)).
assert(wpar(sudamerica,guayana)).
assert(witem(lugar,guayana_francesa)).
assert(wpar(sudamerica,guayana_francesa)).
assert(witem(lugar,uruguay)).
assert(wpar(sudamerica,uruguay)).
assert(witem(lugar,paraguay)).
assert(wpar(sudamerica,paraguay)).
assert(witem(lugar,argentina)).
assert(wpar(sudamerica,argentina)).
assert(witem(lugar,chile)).
assert(wpar(sudamerica,chile)).
assert(witem(lugar,bolivia)).
assert(wpar(sudamerica,bolivia)).
assert(witem(lugar,brasil)).
assert(wpar(sudamerica,brasil)).
assert(witem(lugar,costa_rica)).
assert(wpar(centroamerica,costa_rica)).
assert(witem(lugar,cuba)).
assert(wpar(centroamerica,cuba)).
assert(witem(lugar,ecuador)).
assert(wpar(sudamerica,ecuador)).
assert(witem(lugar,antillas)).
assert(witem(lugar,antillas_mayores)).
assert(wpar(antillas,antillas_mayores)).
assert(witem(lugar,antillas_menores)).
assert(wpar(antillas,antillas_menores)).
assert(witem(lugar,bahamas)).
assert(wpar(antillas,bahamas)).
assert(witem(lugar,la_espanola)).
assert(wpar(antillas_mayores,la_espanola)).
assert(witem(lugar,haiti)).
assert(wpar(la_espanola,haiti)).
assert(witem(lugar,republica_dominicana)).
assert(wpar(la_espanola,republica_dominicana)).
assert(witem(lugar,jamaica)).
assert(wpar(antillas_mayores,jamaica)).
assert(witem(lugar,puerto_rico)).
assert(wpar(antillas_mayores,puerto_rico)).
assert(wpar(antillas_mayores,cuba)).
assert(wpar(centroamerica,antillas)).
retract(wpar(centroamerica,cuba)).
assert(witem(lugar,antigua_y_barbuda)).
assert(wpar(antillas_menores,antigua_y_barbuda)).
assert(witem(lugar,barbados)).
assert(wpar(antillas_menores,barbados)).
assert(witem(lugar,dominica)).
assert(wpar(antillas_menores,dominica)).
assert(witem(lugar,granada)).
assert(wpar(antillas_menores,granada)).
assert(witem(lugar,san_cristobal_y_nieves)).
assert(wpar(antillas_menores,san_cristobal_y_nieves)).
assert(witem(lugar,san_vicente_y_las_granadinas)).
assert(wpar(antillas_menores,san_vicente_y_las_granadinas)).
assert(witem(lugar,santa_lucia)).
assert(wpar(antillas_menores,santa_lucia)).
assert(witem(lugar,trinidad_y_tobago)).
assert(wpar(antillas_menores,trinidad_y_tobago)).
assert(witem(lugar,islas_virgenes_de_estados_unidos)).
assert(wpar(antillas_menores,islas_virgenes_de_estados_unidos)).
assert(witem(lugar,guadalupe)).
assert(wpar(antillas_menores,guadalupe)).
assert(witem(lugar,martinica)).
assert(wpar(antillas_menores,martinica)).
assert(witem(lugar,san_martin)).
assert(wpar(antillas_menores,san_martin)).
assert(witem(lugar,san_bartolome)).
assert(wpar(antillas_menores,san_bartolome)).
assert(witem(lugar,aruba)).
assert(wpar(antillas_menores,aruba)).
assert(witem(lugar,bonaire)).
assert(wpar(antillas_menores,bonaire)).
assert(witem(lugar,curazao)).
assert(wpar(antillas_menores,curazao)).
assert(witem(lugar,saba)).
assert(wpar(antillas_menores,saba)).
assert(witem(lugar,san_eustaquio)).
assert(wpar(antillas_menores,san_eustaquio)).
assert(witem(lugar,sint_maarten)).
assert(wpar(antillas_menores,sint_maarten)).
assert(witem(lugar,anguila)).
assert(wpar(antillas_menores,anguila)).
assert(witem(lugar,islas_virgenes_britanicas)).
assert(wpar(antillas_menores,islas_virgenes_britanicas)).
assert(witem(lugar,montserrat)).
assert(wpar(antillas_menores,montserrat)).
assert(witem(lugar,estado_nueva_esparta)).
assert(wpar(antillas_menores,estado_nueva_esparta)).
assert(witem(lugar,dependencias_federales_venezolanas)).
assert(wpar(antillas_menores,dependencias_federales_venezolanas)).
assert(witem(lugar,francia)).
assert(wpar(francia,guadalupe)).
assert(wpar(francia,martinica)).
assert(wpar(francia,san_martin)).
assert(wpar(francia,san_bartolome)).
assert(witem(lugar,paises_bajos)).
assert(wpar(paises_bajos,aruba)).
assert(wpar(paises_bajos,bonaire)).
assert(wpar(paises_bajos,curazao)).
assert(wpar(paises_bajos,saba)).
assert(wpar(paises_bajos,san_eustaquio)).
assert(wpar(paises_bajos,sint_maarten)).
assert(witem(lugar,reino_unide)).
retract(witem(lugar,reino_unide)).
assert(witem(lugar,reino_unido)).
assert(wpar(reino_unido,anguila)).
assert(wpar(reino_unido,islas_virgenes_britanicas)).
assert(wpar(reino_unido,montserrat)).
assert(wpar(venezuela,estado_nueva_esparta)).
assert(wpar(venezuela,dependencias_federales_venezolanas)).
assert(witem(lugar,estados_unidos)).
assert(wpar(america,estados_unidos)).
assert(wpar(estados_unidos,puerto_rico)).
assert(wpar(estados_unidos,islas_virgenes_de_estados_unidos)).
assert(wpar(norteamerica,estados_unidos)).
assert(witem(lugar,america_latina)).
assert(wpar(america,america_latina)).
assert(wpar(america_latina,mexico)).
assert(wpar(america_latina,centroamerica)).
assert(wpar(america_latina,sudamerica)).
assert(wpar(francia,guayana_francesa)).
assert(witem(lugar,surinam)).
assert(wpar(francia,surinam)).
assert(wpar(sudamerica,surinam)).
assert(witem(lugar,san_pedro_y_miquelon)).
assert(wpar(francia,san_pedro_y_miquelon)).
assert(wpar(norteamerica,san_pedro_y_miquelon)).
retract(wpar(francia,surinam)).
assert(witem(lugar,guayana_esequiba)).
assert(wpar(guayana,guayana_esequiba)).
assert(wpar(venezuela,guayana_esequiba)).
assert(witem(lugar,belice)).
assert(wpar(centroamerica,belice)).
assert(wlist(gente)).
assert(witem(gente,carlos_fuentes)).
assert(wrel('.'(mexico,'.'('','.'(carlos_fuentes,'.'('','.'('',[]))))))).
assert(witem(lugar,isla_de_la_gonave)).
assert(wpar(haiti,isla_de_la_gonave)).
assert(witem(lugar,isla_de_la_tortuga)).
assert(wpar(haiti,isla_de_la_tortuga)).
assert(witem(lugar,islas_cayemites)).
assert(wpar(haiti,islas_cayemites)).
assert(witem(lugar,isla_de_vaches)).
assert(wpar(haiti,isla_de_vaches)).
assert(witem(economia,sector_secundario)).
assert(witem(economia,sector_terciario)).
assert(witem(economia,sector_cuaternario)).
assert(wlist(idioma)).
assert(wlist(tiempo)).
assert(witem(tiempo,edad_antigua)).
assert(witem(tiempo,edad_media)).
assert(witem(tiempo,edad_moderna)).
assert(witem(tiempo,alta_edad_media)).
assert(wpar(edad_media,alta_edad_media)).
assert(witem(tiempo,baja_edad_media)).
assert(wpar(edad_media,baja_edad_media)).
assert(witem(tiempo,edad_contemporanea)).
assert(witem(idioma,castellano_antiguo)).
assert(witem(idioma,latin_vulgar)).
assert(witem(tiempo,edad_del_imperio_romano)).
assert(wpar(edad_antigua,edad_del_imperio_romano)).
assert(wrel('.'('','.'('','.'('','.'(latin_vulgar,'.'(edad_del_imperio_romano,[]))))))).
assert(witem(lugar,imperio_romano)).
assert(wrel('.'(imperio_romano,'.'('','.'('','.'('','.'(edad_del_imperio_romano,[]))))))).
assert(witem(lugar,al_andalus)).
assert(wrel('.'(al_andalus,'.'('','.'('','.'('','.'(edad_media,[]))))))).
assert(witem(idioma,mozarabe)).
assert(wrel('.'(al_andalus,'.'('','.'('','.'(mozarabe,'.'(edad_media,[]))))))).
assert(witem(idioma,bereber)).
assert(witem(idioma,arabe)).
assert(wrel('.'(al_andalus,'.'('','.'('','.'(arabe,'.'(edad_media,[]))))))).
assert(wrel('.'(al_andalus,'.'('','.'('','.'(bereber,'.'(edad_media,[]))))))).
assert(witem(lugar,francia_continental)).
assert(wpar(francia,francia_continental)).
assert(witem(lugar,europa)).
assert(wpar(europa,francia_continental)).
assert(wpar(europa,al_andalus)).
assert(witem(idioma,indoeuropeo)).
assert(witem(idioma,italico)).
assert(wpar(indoeuropeo,italico)).
assert(wpar(italico,latin_vulgar)).
assert(witem(idioma,romance)).
assert(wpar(italico,romance)).
assert(witem(idioma,catalan)).
assert(wpar(romance,catalan)).
assert(witem(idioma,navarroaragones)).
assert(wpar(romance,navarroaragones)).
assert(wrel('.'('','.'('','.'('','.'(navarroaragones,'.'(edad_media,[]))))))).
assert(witem(idioma,asturleones)).
assert(wpar(romance,asturleones)).
assert(witem(idioma,galaicoportugues)).
assert(wpar(romance,galaicoportugues)).
assert(wrel('.'('','.'('','.'('','.'(galaicoportugues,'.'(edad_media,[]))))))).
assert(wpar(romance,mozarabe)).
assert(wpar(romance,castellano_antiguo)).
assert(witem(idioma,afroasiatico)).
assert(witem(idioma,semitico)).
assert(wpar(afroasiatico,semitico)).
assert(wpar(afroasiatico,bereber)).
assert(wpar(semitico,arabe)).
assert(wrel('.'('','.'('','.'('','.'(castellano_antiguo,'.'(baja_edad_media,[]))))))).
assert(witem(idioma,euskera)).
assert(witem(idioma,protoeuskera)).
assert(wpar(euskera,protoeuskera)).
assert(witem(tiempo,siglo_11)).
assert(wpar(baja_edad_media,siglo_11)).
assert(witem(tiempo,siglo_12)).
assert(wpar(baja_edad_media,siglo_12)).
assert(witem(tiempo,siglo_13)).
assert(wpar(baja_edad_media,siglo_13)).
assert(witem(tiempo,siglo_14)).
assert(wpar(baja_edad_media,siglo_14)).
assert(witem(tiempo,siglo_15)).
assert(wpar(baja_edad_media,siglo_15)).
assert(witem(tiempo,siglo_5)).
assert(wpar(alta_edad_media,siglo_5)).
assert(witem(tiempo,siglo_6)).
assert(wpar(alta_edad_media,siglo_6)).
assert(witem(tiempo,siglo_7)).
assert(wpar(alta_edad_media,siglo_7)).
assert(witem(tiempo,siglo_8)).
assert(wpar(alta_edad_media,siglo_8)).
assert(witem(tiempo,siglo_9)).
assert(wpar(alta_edad_media,siglo_9)).
assert(witem(tiempo,siglo_16)).
assert(wpar(edad_moderna,siglo_16)).
assert(witem(tiempo,siglo_17)).
assert(wpar(edad_moderna,siglo_17)).
assert(witem(tiempo,siglo_18)).
assert(wpar(edad_moderna,siglo_18)).
assert(witem(tiempo,siglo_19)).
assert(wpar(edad_contemporanea,siglo_19)).
assert(witem(tiempo,siglo_20)).
assert(wpar(edad_contemporanea,siglo_20)).
assert(witem(tiempo,siglo_21)).
assert(wpar(edad_contemporanea,siglo_21)).
assert(witem(idioma,espanol)).
assert(wpar(romance,espanol)).
assert(wrel('.'(bolivia,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(costa_rica,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(cuba,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(ecuador,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(witem(lugar,el_salvador)).
assert(wpar(centroamerica,el_salvador)).
assert(wrel('.'(el_salvador,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(guatemala,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(honduras,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(nicaragua,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(panama,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(paraguay,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(witem(idioma,guarani)).
assert(wrel('.'(paraguay,'.'('','.'('','.'(guarani,'.'('',[]))))))).
assert(witem(idioma,tupi)).
assert(witem(idioma,quechua)).
assert(witem(idioma,aimara)).
assert(wpar(tupi,guarani)).
assert(witem(lugar,peru)).
assert(wpar(sudamerica,peru)).
assert(witem(idioma,chibcha)).
assert(witem(idioma,mapuche)).
assert(wrel('.'(argentina,'.'('','.'('','.'(mapuche,'.'('',[]))))))).
assert(wrel('.'(chile,'.'('','.'('','.'(mapuche,'.'('',[]))))))).
assert(witem(idioma,maya)).
assert(witem(idioma,cholano)).
assert(wpar(maya,cholano)).
assert(witem(lugar,chiapas)).
assert(wpar(mexico,chiapas)).
assert(wrel('.'(chiapas,'.'('','.'('','.'(cholano,'.'('',[]))))))).
assert(witem(idioma,azteca)).
assert(witem(idioma,nahuatl)).
assert(wpar(azteca,nahuatl)).
assert(wrel('.'(mexico,'.'('','.'('','.'(nahuatl,'.'('',[]))))))).
assert(wrel('.'(argentina,'.'('','.'('','.'(quechua,'.'('',[]))))))).
assert(wrel('.'(bolivia,'.'('','.'('','.'(quechua,'.'('',[]))))))).
assert(wrel('.'(colombia,'.'('','.'('','.'(quechua,'.'('',[]))))))).
assert(wrel('.'(chile,'.'('','.'('','.'(quechua,'.'('',[]))))))).
assert(wrel('.'(ecuador,'.'('','.'('','.'(quechua,'.'('',[]))))))).
assert(wrel('.'(peru,'.'('','.'('','.'(quechua,'.'('',[]))))))).
assert(wrel('.'(peru,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(peru,'.'('','.'('','.'(aimara,'.'('',[]))))))).
assert(wrel('.'(republica_dominicana,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(venezuela,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(mexico,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(argentina,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(puerto_rico,'.'('','.'('','.'(espanol,'.'('',[]))))))).
assert(wrel('.'(uruguay,'.'('','.'('','.'(espanol,'.'('',[]))))))).
retract(wpar(norteamerica,estados_unidos)).
assert(witem(lugar,estados_unidos_continentales)).
assert(wpar(estados_unidos,estados_unidos_continentales)).
assert(wpar(norteamerica,estados_unidos_continentales)).
assert(witem(idioma,espanol_neomexicano)).
assert(wpar(espanol,espanol_neomexicano)).
assert(witem(lugar,nuevo_mexico)).
assert(wpar(estados_unidos_continentales,nuevo_mexico)).
assert(wrel('.'(nuevo_mexico,'.'('','.'('','.'(espanol_neomexicano,'.'('',[]))))))).
assert(witem(lugar,tierras_costeras)).
assert(wpar(america_latina,tierras_costeras)).
assert(witem(lugar,tierras_interiores)).
assert(wpar(america_latina,tierras_interiores)).
assert(wlist(unidad_linguistica)).
assert(witem(unidad_linguistica,sonido)).
assert(witem(unidad_linguistica,fonema)).
assert(witem(unidad_linguistica,morfema)).
assert(witem(unidad_linguistica,lexema)).
assert(witem(unidad_linguistica,palabra)).
assert(witem(unidad_linguistica,sintagma)).
assert(witem(unidad_linguistica,consonante)).
assert(wpar(fonema,consonante)).
assert(witem(unidad_linguistica,vocal)).
assert(wpar(fonema,vocal)).
assert(witem(unidad_linguistica,silaba)).
assert(wlist(punto_de_articulacion)).
assert(wlist(modo_de_articulacion)).
assert(wlist(sonoridad)).
assert(witem(unidad_linguistica,p)).
assert(wpar(consonante,p)).
assert(witem(unidad_linguistica,b)).
assert(wpar(consonante,b)).
assert(witem(unidad_linguistica,t)).
assert(wpar(consonante,t)).
assert(witem(unidad_linguistica,d)).
assert(wpar(consonante,d)).
assert(witem(unidad_linguistica,f)).
assert(wpar(consonante,f)).
assert(witem(unidad_linguistica,v)).
assert(wpar(consonante,v)).
assert(witem(unidad_linguistica,th)).
assert(wpar(consonante,th)).
assert(witem(unidad_linguistica,s)).
assert(wpar(consonante,s)).
assert(witem(unidad_linguistica,ch)).
assert(wpar(consonante,ch)).
assert(witem(unidad_linguistica,z)).
assert(wpar(consonante,z)).
assert(witem(unidad_linguistica,r)).
assert(wpar(consonante,r)).
assert(witem(unidad_linguistica,k)).
assert(wpar(consonante,k)).
assert(witem(unidad_linguistica,g)).
assert(wpar(consonante,g)).
assert(witem(unidad_linguistica,x)).
assert(wpar(consonante,x)).
assert(witem(unidad_linguistica,ly)).
assert(wpar(consonante,ly)).
assert(witem(unidad_linguistica,ny)).
assert(wpar(consonante,ny)).
assert(witem(unidad_linguistica,m)).
assert(wpar(consonante,m)).
assert(witem(punto_de_articulacion,bilabial)).
assert(witem(punto_de_articulacion,labiodental)).
assert(witem(punto_de_articulacion,ldental)).
assert(witem(punto_de_articulacion,dental)).
assert(witem(punto_de_articulacion,interdental)).
assert(witem(punto_de_articulacion,alveolar)).
assert(witem(punto_de_articulacion,postalveolar)).
assert(witem(punto_de_articulacion,retrofleja)).
assert(witem(punto_de_articulacion,palatal)).
assert(witem(punto_de_articulacion,velar)).
assert(witem(punto_de_articulacion,uvular)).
assert(witem(punto_de_articulacion,glotal)).
assert(witem(unidad_linguistica,pronombre)).
assert(wpar(palabra,pronombre)).
assert(witem(unidad_linguistica,pronombre_personal)).
assert(wpar(pronombre,pronombre_personal)).
assert(witem(unidad_linguistica,tu)).
assert(wpar(pronombre_personal,tu)).
assert(witem(unidad_linguistica,vos)).
assert(wpar(pronombre_personal,vos)).
assert(witem(unidad_linguistica,usted)).
assert(wpar(pronombre_personal,usted)).
assert(witem(unidad_linguistica,ustedes)).
assert(wpar(pronombre_personal,ustedes)).
assert(wlist(trato)).
assert(witem(trato,trato_de_confianza)).
assert(witem(trato,trato_de_respecto)).
assert(witem(idioma,espanol_de_las_tierras_costeras)).
assert(wpar(espanol,espanol_de_las_tierras_costeras)).
assert(witem(idioma,espanol_de_las_tierras_interiores)).
assert(wpar(espanol,espanol_de_las_tierras_interiores)).
assert(witem(idioma,espanol_rioplatense)).
assert(wpar(espanol,espanol_rioplatense)).
assert(wrel('.'(argentina,'.'('','.'('','.'(espanol_rioplatense,'.'('','.'('','.'('','.'('','.'('','.'('',[])))))))))))).
assert(wrel('.'(argentina,'.'('','.'('','.'('','.'('','.'(vos,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(america_latina,'.'('','.'('','.'('','.'('','.'(ustedes,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(witem(unidad_linguistica,vosotros)).
assert(wpar(pronombre_personal,vosotros)).
assert(witem(lugar,espana_peninsular)).
assert(wpar(europa,espana_peninsular)).
assert(witem(lugar,espana)).
assert(wpar(espana,espana_peninsular)).
assert(wrel('.'(espana,'.'('','.'('','.'('','.'('','.'(vosotros,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(cuba,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(bolivia,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(chile,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(espana,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(mexico,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(panama,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(peru,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(puerto_rico,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(republica_dominicana,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(venezuela,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(colombia,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(ecuador,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(guatemala,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(costa_rica,'.'('','.'('','.'('','.'('','.'(usted,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(colombia,'.'('','.'('','.'('','.'('','.'(vos,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(colombia,'.'('','.'('','.'('','.'('','.'(usted,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(el_salvador,'.'('','.'('','.'('','.'('','.'(vos,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(guatemala,'.'('','.'('','.'('','.'('','.'(vos,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(honduras,'.'('','.'('','.'('','.'('','.'(vos,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(honduras,'.'('','.'('','.'('','.'('','.'(tu,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(chiapas,'.'('','.'('','.'('','.'('','.'(vos,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(nicaragua,'.'('','.'('','.'('','.'('','.'(vos,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(paraguay,'.'('','.'('','.'('','.'('','.'(vos,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wrel('.'(uruguay,'.'('','.'('','.'('','.'('','.'(vos,'.'('','.'('','.'('','.'(trato_de_confianza,[])))))))))))).
assert(wlist(proceso)).
assert(witem(proceso,proceso_fonologico)).
assert(witem(proceso,aspiracion_de_la_s_implosiva)).
assert(wpar(proceso_fonologico,aspiracion_de_la_s_implosiva)).
assert(wrel('.'(centroamerica,'.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'(aspiracion_de_la_s_implosiva,[]))))))))))))).
assert(wrel('.'(nicaragua,'.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'(aspiracion_de_la_s_implosiva,[]))))))))))))).
assert(witem(proceso,velarizacion_de_las_oclusivas_sordas)).
assert(wpar(proceso_fonologico,velarizacion_de_las_oclusivas_sordas)).
assert(wrel('.'(centroamerica,'.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'(velarizacion_de_las_oclusivas_sordas,[]))))))))))))).
assert(witem(proceso,confusion_de_liquidas)).
assert(wpar(proceso_fonologico,confusion_de_liquidas)).
assert(wrel('.'(antillas,'.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'(confusion_de_liquidas,[]))))))))))))).
assert(wrel('.'(chile,'.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'(aspiracion_de_la_s_implosiva,[]))))))))))))).
assert(witem(proceso,seseo)).
assert(wrel('.'(chile,'.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'(seseo,[]))))))))))))).
assert(witem(proceso,yeismo)).
assert(wrel('.'(chile,'.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'(yeismo,[]))))))))))))).
assert(witem(proceso,voseo_chileno)).
assert(wrel('.'(chile,'.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'('','.'(voseo_chileno,[]))))))))))))).
assert(wrel('.'(espana,'.'('','.'('','.'(espanol,'.'('','.'('','.'('','.'('','.'('','.'('','.'('',[]))))))))))))).
retract(wrel('.'('','.'('','.'('','.'(galaicoportugues,'.'(edad_media,[]))))))).
assert(witem(lugar,galicia)).
assert(wpar(espana,galicia)).
assert(wrel('.'(galicia,'.'('','.'('','.'(galaicoportugues,'.'(edad_media,'.'('','.'('','.'('','.'('','.'('','.'('',[]))))))))))))).
