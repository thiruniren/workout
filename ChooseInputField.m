(* ::Package:: *)

BeginPackage["ChooseInputField`"];
(*
Author: Thirupathi
Date: 9/Jan/2012
Package Name: ThirupathiChooseInputFieldPackage
Package Description: ChooseInputField as per user requirement.
*)

(*Initialization of variables*)
popupMenuValue=0;
buttonAction=1;
greenColor=0;
mouseClicked=0;
labelValue=0;
backgroundColor=False;
boxColor2=False;
enableRow=False;
labelBackground=Black;
flag=False;
(*
Function Name: initializeList
Input: noOfRows as firstDimension and noOfInputFields as secondDimension as per user Requirement
Output: Make inputVariable as Null
Function Description: Initialize inputVariable to Null values
*)
initializeList[firstDimension_,secondDimension_]:=
					If[firstDimension>0&&secondDimension>0&&NumberQ[firstDimension]&&NumberQ[secondDimension],
						(
							For[firstIteration=1,firstIteration<=firstDimension,firstIteration++,
														For[secondIteration=1,secondIteration<=secondDimension,secondIteration++,
																							inputVariable[firstIteration,secondIteration]=Null
															]
								];
						),
						Throw[Print["Value is incorrect. Enter correct value"]]
					]
(*
Function Name: inputBox
Input: individualInputField,individualLabelRow
Output: Created inptuBox and Particular InputBox will be selected by user in particular row.
Function Description: To create and to select particular inputbox.
*)
inputBox[individualInputField2_,individualLabelRow2_]:=
												If[individualInputField2>0&&individualLabelRow2>0&&NumberQ[individualInputField2]&&NumberQ[individualLabelRow2],
													DynamicModule[
																	{},
																	(*Make flag as true if buttonAction(Choose Button) is 0*)
																	If[
																		buttonAction==0,flag=True
																	   ];
																	(*When mouse move from one box to another box make sure green color move corresponding to mouse and 
																	When mouse pressed on particular box, make flag as False,mouseClicked as 1,buttonAction as 1*)
																	EventHandler[
																					InputField[
																								Dynamic[inputVariable[individualLabelRow2,individualInputField2]],
																								Appearance->Frameless,ImageSize->30,Enabled->enableRow,ContinuousAction-> True
																							  ],
																					{
																						"MouseMoved":>If[flag,greenColor=individualInputField2],
																						"MouseDown":>(flag=False;mouseClicked=1;buttonAction=1),
																						PassEventsDown->True
																					}
																				]
																],
													Throw[Print["Value is incorrect. Enter correct value"]]
												]
(*
Function Name: labelRow
Input: inputField,individualLabelRow1
Output: Create the row and change the background color to yellow when choose button is pressed then change the particular input box color corresponds to mouse moved
Function Description: To Create individual Row contain InputField
*)
labelRow[inputFieldNo2_,individualLabelRow1_]:=
									If[inputFieldNo2>0&&individualLabelRow1>0&&NumberQ[inputFieldNo2]&&NumberQ[individualLabelRow1],
										Dynamic[
												( (*When label choose from popupmenu, enable & change color of particular label *)
												  If[popupMenuValue!=individualLabelRow1,enableRow=False;gray=.5,enableRow=True;gray=1];
													(*After label been choosen and choose button has clicked then make background color of particular label to yellow *)
												  If[popupMenuValue==individualLabelRow1&&buttonAction==0&&mouseClicked==0,backgroundColor=Yellow;boxColor1=Green,backgroundColor=White;boxColor1=White];
													(*After label background is been in yellow and mouse clicked on particular input field then make boxColor2 to green*)
												  If[popupMenuValue==individualLabelRow1&&buttonAction==1&&mouseClicked==1,boxColor2=Green,boxColor2=False];
													(*After label background is been in yellow, boxColor2 been in green and boxColor1 is White then make boxColor1 is false*)
												  If[popupMenuValue==individualLabelRow1&&buttonAction==1&&mouseClicked==1&&boxColor1==White,boxColor1=False];
													(*when Choose button pressed again, change labelbackground to GrayLevel[.8]*)
												  If[buttonAction==0,labelBackground=GrayLevel[.8],labelBackground=Black];
													(*Construct label and inputfield based on user requirement*)
												  Row[{
														Framed[Style["Label"<>ToString[individualLabelRow1],White,16,Bold,GrayLevel[gray]],Background-> labelBackground,BaseStyle->White],
														Grid[
															  {Table[inputBox[iteration2,individualLabelRow1],{iteration2,1,inputFieldNo2}]},
															  Frame-> All,Background-> {backgroundColor,None,{{1,greenColor}->(boxColor2||boxColor1)}},FrameStyle-> RGBColor[0.478,0.694,0.733]
															]
													   }
													]
												)
											 ],
										Throw[Print["Value you are entered invalid. Enter valid value."]]
									]
(*
Function Name: labelImage
Input: labelRowNo,inputFieldNo
Output: initialization of inputVariable and Construction of Label with inputFields
Function Description: Creation of No of label and make call to initialize inputVariable.
*)
labelImage[labelRowNo2_,inputFieldNo1_]:=
						If[labelRowNo2>0&&inputFieldNo1>0&&NumberQ[labelRowNo2]&&NumberQ[inputFieldNo1],
							Module[{},initializeList[labelRowNo2,inputFieldNo1];Column[Table[labelRow[inputFieldNo1,iteration1],{iteration1,1,labelRowNo2}]]],
							Throw[Print["The value is invalid. Pleade give correct value!"]]
						]
(*
Function Name: labelRow
Input: labelRowNo
Output: PopupMenu With no of Labels that user wants to create.
Function Description: Creation of PopupMenu
*)
popupMenu[labelRowNo1_]:=
					If[labelRowNo1>0&&NumberQ[labelRowNo1],
					Dynamic[
							(*When Choose Button(buttonAction) is pressed and mouseClicked is 0 then disable the popupmenu otherwise enable*)
							If[buttonAction==0&& mouseClicked==0,popupEnable=False;popupMenuGray=.3,popupEnable=True;popupMenuGray=1];
							(*When user click on particular label from popupmenu, number from that label(Label1 or Label5 or something) is been seperated and stored in popupMenuValue*)
							If[labelValue!= 0,popupMenuValue=ToExpression[StringTake[labelValue,{6,6}]]];
							(*Create PopupMenu*)
							PopupMenu[
										(*labelValue is dynamically changing based on value been Choosen by user*)
									   Dynamic[labelValue],
									   Rest@Flatten@Table[{Delimiter,"Label"<>ToString[iteration3]},{iteration3,1,labelRowNo1}],
									   "",
										(*Creation of image of popupmenu and down arrow marker*)
									   Row[{
											Panel[Style["PopupMenu",White,Bold,GrayLevel[popupMenuGray]],Background->RGBColor[0.235,0.486,0.768],Alignment->Center,ImageSize->{90,35}],
											Panel[Style["\[FilledDownTriangle]",White,16,GrayLevel[popupMenuGray]],Background->RGBColor[0.235,0.486,0.768],ImageSize->{30,35},Alignment->Center]
										   }
										 ],
									   MenuStyle->{16,Blue},Alignment->Center,Appearance->"Button",Enabled-> popupEnable
									]
						],
					Throw[Print["Label Value Should be Greater than 0"]]
					]
(*
Function Name: button
Input: No
Output: Created Button
Function Description: Creation of Button.
*)
button[buttonName_]:=
	If[StringQ[buttonName],
		Dynamic[
				(*Whenever a value been selected in popupmenu change the button to enable mode*)
				If[popupMenuValue==0,buttonEnabled=False;buttonGray=.3,buttonEnabled=True;buttonGray=1];
				(*Creation of button*)
				Button[
						Style[buttonName,White,GrayLevel[buttonGray]],
						(buttonAction=Replace[buttonAction,{0->1,1->0}];mouseClicked=0;greenColor=0),
						Enabled-> buttonEnabled,Background-> RGBColor[0.235,0.486,0.768],Appearance->"Palette",ImageSize->{80,35}
					  ]
			  ],
		Throw[Print["Enter String Only"]]
		];

EndPackage[];
