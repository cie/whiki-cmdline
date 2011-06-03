assert(wlist(ido)).
assert(witem(ido,jun10)).
assert(witem(ido,jun11)).
assert(witem(ido,jun12)).
assert(witem(ido,jun13)).
assert(witem(ido,jun14)).
assert(witem(ido,jun15)).
assert(witem(ido,jun16)).
assert(wlist(idojaras)).
assert(witem(idojaras,napos)).
assert(witem(idojaras,esos)).
assert(witem(idojaras,hideg)).
assert(wlist(cucc)).
assert(witem(cucc,ruha)).
assert(witem(cucc,cipo)).
assert(wpar(ruha,cipo)).
assert(witem(cucc,esokabat)).
assert(wpar(ruha,esokabat)).
assert(wrel('.'('','.'(esos,'.'(esokabat,[]))))).
assert(wlist(program)).
assert(witem(program,tini_klub)).
assert(witem(program,jatek)).
assert(wpar(tini_klub,jatek)).
assert(witem(program,furdes)).
assert(wpar(tini_klub,furdes)).
assert(witem(program,kajalas)).
assert(wpar(tini_klub,kajalas)).
assert(witem(program,dicsoites)).
assert(wpar(tini_klub,dicsoites)).
assert(witem(program,beszelgetes)).
assert(wpar(tini_klub,beszelgetes)).
assert(witem(program,ismerkedos_jatek)).
assert(wpar(jatek,ismerkedos_jatek)).
assert(wlist(hely)).
assert(witem(hely,velence)).
assert(witem(hely,gyuli)).
assert(wpar(velence,gyuli)).
assert(witem(hely,to)).
assert(wpar(velence,to)).
assert(wrel('.'('','.'('','.'('','.'(furdes,'.'(to,[]))))))).
assert(wrel('.'('','.'(napos,'.'('','.'(furdes,'.'('',[]))))))).
assert(witem(program,ctf)).
assert(wpar(jatek,ctf)).
assert(witem(hely,park)).
assert(wpar(velence,park)).
assert(wrel('.'('','.'('','.'('','.'(ctf,'.'(park,[]))))))).
assert(wrel('.'('','.'(esos,'.'('','.'('','.'(gyuli,[]))))))).
assert(wrel('.'('','.'(hideg,'.'('','.'('','.'(gyuli,[]))))))).
assert(wrel('.'(jun12,'.'('','.'('','.'(tini_klub,'.'('',[]))))))).
assert(wrel('.'(jun12,'.'(napos,'.'('','.'('','.'('',[]))))))).
assert(wrel('.'(jun12,'.'(esos,'.'('','.'('','.'('',[]))))))).
retract(wrel('.'('','.'('','.'('','.'(furdes,'.'(to,[]))))))).
retract(wrel('.'('','.'(napos,'.'('','.'(furdes,'.'('',[]))))))).
assert(wrel('.'('','.'(napos,'.'('','.'(furdes,'.'(to,[]))))))).
retract(wrel('.'('','.'(napos,'.'('','.'(furdes,'.'(to,[]))))))).
assert(witem(cucc,furdoruha)).
assert(wrel('.'(jun12,'.'(napos,'.'(furdoruha,'.'(furdes,'.'(to,[]))))))).
assert(wrel('.'('','.'(napos,'.'('','.'(ctf,'.'('',[]))))))).
retract(wrel('.'(jun12,'.'('','.'('','.'(tini_klub,'.'('',[]))))))).
assert(wrel('.'(jun12,'.'('','.'('','.'(tini_klub,'.'(velence,[]))))))).
retract(wrel('.'('','.'('','.'('','.'(ctf,'.'(park,[]))))))).
retract(wrel('.'('','.'(napos,'.'('','.'(ctf,'.'('',[]))))))).
assert(wrel('.'(jun12,'.'(napos,'.'('','.'(ctf,'.'(park,[]))))))).
assert(wpar(ruha,furdoruha)).
assert(witem(cucc,kaja)).
assert(wrel('.'(jun12,'.'(napos,'.'(kaja,'.'(kajalas,'.'(to,[]))))))).
assert(wrel('.'(jun12,'.'(esos,'.'(kaja,'.'(kajalas,'.'(gyuli,[]))))))).
retract(wrel('.'('','.'(esos,'.'('','.'('','.'(gyuli,[]))))))).
retract(wrel('.'('','.'(hideg,'.'('','.'('','.'(gyuli,[]))))))).
assert(wrel('.'('','.'(hideg,'.'('','.'('','.'(gyuli,[]))))))).
retract(wrel('.'(jun12,'.'(napos,'.'('','.'(ctf,'.'(park,[]))))))).
assert(witem(cucc,ctf_cucc)).
assert(wrel('.'(jun12,'.'(napos,'.'(ctf_cucc,'.'(ctf,'.'(park,[]))))))).
assert(witem(program,utazas)).
assert(wpar(tini_klub,utazas)).
assert(witem(hely,pusztaszabolcs)).
assert(witem(cucc,penz)).
assert(witem(ido,jun12_del)).
assert(wpar(jun12,jun12_del)).
assert(wrel('.'(jun12_del,'.'('','.'(penz,'.'(utazas,'.'(pusztaszabolcs,[]))))))).
assert(witem(cucc,gitar)).
assert(wrel('.'('','.'('','.'(gitar,'.'(dicsoites,'.'('',[]))))))).
assert(witem(cucc,keksz)).
assert(wpar(kaja,keksz)).
assert(witem(cucc,kekszrevalo)).
assert(wpar(kaja,kekszrevalo)).
assert(witem(cucc,zaszlok)).
assert(wpar(ctf_cucc,zaszlok)).
assert(witem(cucc,kotel)).
assert(wpar(ctf_cucc,kotel)).
assert(witem(cucc,satorcovek)).
assert(wpar(ctf_cucc,satorcovek)).
assert(witem(cucc,sator)).
assert(witem(cucc,ponyva)).
assert(wpar(sator,ponyva)).
assert(wpar(sator,kotel)).
assert(wpar(sator,satorcovek)).
assert(witem(program,korhinta)).
assert(wpar(ismerkedos_jatek,korhinta)).
assert(witem(cucc,korhinta_kerdesek)).
assert(wrel('.'('','.'('','.'(korhinta_kerdesek,'.'(korhinta,'.'('',[]))))))).
assert(witem(cucc,kotta)).
assert(wrel('.'('','.'('','.'(kotta,'.'(dicsoites,'.'('',[]))))))).
assert(witem(cucc,furulya)).
assert(wrel('.'('','.'('','.'(furulya,'.'(dicsoites,'.'('',[]))))))).
assert(witem(program,beszelgetes_meserol)).
assert(wpar(beszelgetes,beszelgetes_meserol)).
assert(witem(program,beszelgetes_bibliarol)).
assert(wpar(beszelgetes,beszelgetes_bibliarol)).
assert(witem(cucc,mese)).
assert(witem(cucc,biblia)).
assert(wrel('.'('','.'('','.'(mese,'.'(beszelgetes_meserol,'.'('',[]))))))).
assert(wrel('.'('','.'('','.'(biblia,'.'(beszelgetes_bibliarol,'.'('',[]))))))).
assert(witem(program,evolucio)).
assert(wpar(ismerkedos_jatek,evolucio)).
assert(witem(program,szurkolos)).
assert(wpar(ismerkedos_jatek,szurkolos)).
assert(witem(program,maffia)).
assert(wpar(jatek,maffia)).
assert(witem(program,nomic)).
assert(wpar(jatek,nomic)).
assert(witem(cucc,nomic_cucc)).
assert(witem(cucc,kezdeti_szabalyok)).
assert(wpar(nomic_cucc,kezdeti_szabalyok)).
assert(witem(cucc,toll)).
assert(wpar(nomic_cucc,toll)).
assert(witem(cucc,ceruza)).
assert(wpar(nomic_cucc,ceruza)).
assert(witem(cucc,papirok)).
assert(wpar(nomic_cucc,papirok)).
assert(witem(program,vezetoi_megbeszeles)).
