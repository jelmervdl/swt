%%% This output was generated by the following command:
%%% candc/bin/boxer --stdin --box true --resolve true --instantiate true --roles proto 

:- multifile     sem/3, id/2.
:- discontiguous sem/3, id/2.
:- dynamic       sem/3, id/2.
%%% Who has directed the most movies starring Johnny Depp ? 
id(1,1).
sem(1,
	[
		1001:[tok:'Who',pos:'WP',lemma:who,namex:'O'],
		1002:[tok:has,pos:'VBZ',lemma:have,namex:'O'],
		1003:[tok:directed,pos:'VBN',lemma:direct,namex:'O'],
		1004:[tok:the,pos:'DT',lemma:the,namex:'O'],
		1005:[tok:most,pos:'RBS',lemma:most,namex:'O'],
		1006:[tok:movies,pos:'NNS',lemma:movie,namex:'O'],
		1007:[tok:starring,pos:'VBG',lemma:star,namex:'O'],
		1008:[tok:'Johnny',pos:'NNP',lemma:'Johnny',namex:'I-PER'],
		1009:[tok:'Depp',pos:'NNP',lemma:'Depp',namex:'I-PER'],
		1010:[tok: ?,pos:'.',lemma: ?,namex:'O']
	],
	alfa(
		top,
		drs(
			[[1004]:x0,[]:x1],
			[
				[1006]:pred(x0,movie,n,0),
				[1005]:pred(x0,most,a,0),
				[1008,1009]:named(x1,johnny_depp,per,0)
			]
		),
		drs(
			[],
			[
				[]:whq(
					[ins:hum],
					drs(
						[[]:x2],
						[
							[1001]:pred(x2,person,n,1)
						]
					),
					x2,
					drs(
						[[]:x3,[]:x4],
						[
							[]:rel(x3,x1,patient,0),
							[]:rel(x3,x0,agent,0),
							[1007]:pred(x3,star,v,0),
							[]:rel(x4,x0,patient,0),
							[]:rel(x4,x2,agent,0),
							[1003]:pred(x4,direct,v,0)
						]
					)
				)
			]
		)
	)
).

%%%   _________________________   _______________________________  
%%%  |x0 x1                    | |                               | 
%%%  |.........................| |...............................| 
%%% (|movie(x0)                |A|  __________   ______________  |)
%%%  |most(x0)                 | | |x2        | |x3 x4         | | 
%%%  |named(x1,johnny_depp,per)| | |..........| |..............| | 
%%%  |_________________________| | |person(x2)|?|patient(x3,x1)| | 
%%%                              | |__________| |agent(x3,x0)  | | 
%%%                              |              |star(x3)      | | 
%%%                              |              |patient(x4,x0)| | 
%%%                              |              |agent(x4,x2)  | | 
%%%                              |              |direct(x4)    | | 
%%%                              |              |______________| | 
%%%                              |_______________________________| 


