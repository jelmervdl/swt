%%% This output was generated by the following command:
%%% bin/boxer --stdin --box true --resolve true 

:- multifile     sem/3, id/2.
:- discontiguous sem/3, id/2.
:- dynamic       sem/3, id/2.
%%% Who directed Django Unchained ? 
id(1,1).
sem(1,
	[
		1001:[tok:'Who',pos:'WP',lemma:who,namex:'O'],
		1002:[tok:directed,pos:'VBD',lemma:direct,namex:'O'],
		1003:[tok:'Django',pos:'NNP',lemma:'Django',namex:'I-ORG'],
		1004:[tok:'Unchained',pos:'NNP',lemma:'Unchained',namex:'I-ORG'],
		1005:[tok: ?,pos:'.',lemma: ?,namex:'O']
	],
	alfa(
		top,
		drs(
			[[]:x0],
			[
				[1003,1004]:named(x0,django_unchained,org,0)
			]
		),
		drs(
			[],
			[
				[]:whq(
					[ins:hum],
					drs(
						[[]:x1],
						[
							[1001]:pred(x1,person,n,1)
						]
					),
					x1,
					drs(
						[[]:x2],
						[
							[]:rel(x2,x0,patient,0),
							[]:rel(x2,x1,agent,0),
							[1002]:pred(x2,direct,v,0)
						]
					)
				)
			]
		)
	)
).
%%%   ______________________________   _______________________________  
%%%  |x0                            | |                               | 
%%%  |..............................| |...............................| 
%%% (|named(x0,django_unchained,org)|A|  __________   ______________  |)
%%%  |______________________________| | |x1        | |x2            | | 
%%%                                   | |..........| |..............| | 
%%%                                   | |person(x1)|?|patient(x2,x0)| | 
%%%                                   | |__________| |agent(x2,x1)  | | 
%%%                                   |              |direct(x2)    | | 
%%%                                   |              |______________| | 
%%%                                   |_______________________________| 


