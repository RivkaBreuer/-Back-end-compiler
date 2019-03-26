-- Miri Waisboam 304915051
-- Riki Breuer 203167416
--150060.37.5779.41

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Directories;
use ada.Directories;
with ada.Strings;
use ada.Strings;
with ada.Strings.Fixed;
use ada.Strings.Fixed;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with GNAT.Array_Split;
with GNAT.String_Split;




procedure Main is

 package SU renames ada.Strings.Unbounded;
   package D renames ada.Directories;

 my_file, my_newfile, d_file : File_Type;
   m,ll,l: Unbounded_String;
   w,wo: Unbounded_String;
   b: Character:='j';
   word1,word3,word2:Unbounded_String;
    className, tType, kind, Name, symb,tempWord: Unbounded_String;
   arg , var: Integer:=0;
   classScopeIndex,methodScopeIndex :Integer:=1;
   argumentIndex, varIndex,staticIndex, fieldIndex,  functionKind:Integer:=0;
   counterParam,counterIf, counterWhile:Integer:=0;
   isElse, jump :Integer:=0;
   enterLoop,f, num, numsParam,lent: integer;
   temp1,temp,im: Unbounded_String;
   functionCall,functionCall1,functionCall2,tt,t,type1,op,n:Unbounded_String;

   type string_Display is
     array (Positive range <>, Positive range <>) of Unbounded_String;

   classScope: string_Display(1..5,1..500);
   methodScope:string_Display(1..5,1..500);

  -- b:=Magic_Square(1,1);



   dd:String:= "C:\AAA EKRONOT\Targil 5 b\dir.txt";

  function f_keyword (s:string) return Unbounded_String is
   begin
      if s="class" or s="constructor"or s="function"or s="method"or s="field"or s="static"or s="var"or s="int"or s="char"or s="boolean"or s="void"or s="true"or s="false"or s="null"or s="this"or s="let"or s="do"or s="if"or s="else"or s="while"or s="return" then
         return To_Unbounded_String(s);
         else
      return To_Unbounded_String("No");
      end if;
   end f_keyword;

   function f_symbol (b:Character) return Character is
   begin
      if b='('or b=')'or b='{'or b='}'or b='['or b=']'or b='.'or b=','or b=';'or b='+'or b='-'or b='*' or b='&'or b='|'or b='<'or b='>'or b='='or b='~' then
         return b;
         else
   return '!';
   end if;
   end f_symbol;






   procedure pr_Class(my_newfile : File_Type);
   procedure pr_ClassVarDec(my_newfile: File_Type) ;
   procedure pr_SubroutineDec(my_newfile: File_Type) ;
   procedure pr_ParameterList(my_newfile: File_Type);
   procedure pr_SubroutineBody(my_newfile: File_Type);
   procedure  pr_VarDec(my_newfile: File_Type);
   procedure pr_Statement(my_newfile: File_Type);
   procedure pr_Statements(my_newfile: File_Type);
   procedure pr_LetStatement(my_newfile: File_Type);
   procedure pr_IfStatement(my_newfile: File_Type);
   procedure pr_ReturnStatement(my_newfile: File_Type);
   procedure pr_WhileStatement(my_newfile: File_Type);
     procedure pr_DoStatement(my_newfile: File_Type);
   procedure pr_ExpressionList(my_newfile: File_Type);
    procedure pr_Term(my_newfile: File_Type;ll:Unbounded_String);

function getValue(Line: in Unbounded_String) return Unbounded_String is
    Subs: GNAT.String_Split.Slice_Set;
      Seps: String:= "< " & " >";
      TheValue: Unbounded_String;
   begin
     GNAT.String_Split.Create(s=> Subs,From => To_String(Line),Separators => Seps,
                            Mode=> GNAT.String_Split.Multiple);
      TheValue:= To_Unbounded_String(GNAT.String_Split.Slice(Subs,3));
      return TheValue;
      end getValue;

function f_find(name:Unbounded_String) return integer is

		i:Integer:=1;
		found:Integer:=0;


   begin
      --      kind:=To_Unbounded_String("");
      num:=0;
      while ((i <= methodScopeIndex) and (found = 0)) loop

			if(methodScope(1,i)= name) then
                               found:=1;

				kind:=methodScope(3,i);
				if(kind = To_Unbounded_String("var")) then
				    kind:=To_Unbounded_String("local");
				end if;
            num:=integer'Value(To_String(methodScope(4,i))); --Put_Line(Integer'Image(num));
				ttype:=methodScope(2,i);
			end if;
			i:=i+1;
		end loop;
		i:=1;
		while ((i <= classScopeIndex) and (found = 0)) loop
			if(classScope(1,i) = name) then
				found :=1;
				ttype:=classScope(2,i);
				if(classScope(3,i)= To_Unbounded_String("static")) then
					kind:=To_Unbounded_String("static");
				else kind:=To_Unbounded_String("this");
               num:=integer'Value(To_String(classScope(4,i)));
              -- Put_Line(Integer'Image(num));
				end if;
			end if;
			i:=i+1;
		end loop;
		return found;
	end f_find;

procedure pr_Expression(my_newfile: File_Type; sl:String:="") is

   begin
   --  Put_Line(my_newfile,sl);

      pr_Term(my_newfile,To_Unbounded_String(sl));
      while l=To_Unbounded_String("<symbol> + </symbol>")
        or l=To_Unbounded_String("<symbol> * </symbol>")
        or l=To_Unbounded_String("<symbol> / </symbol>")
        or l=To_Unbounded_String("<symbol> &gt; </symbol>")
        or l=To_Unbounded_String("<symbol> - </symbol>")
        or l=To_Unbounded_String("<symbol> | </symbol>")
        or l=To_Unbounded_String("<symbol> = </symbol>")
        or l=To_Unbounded_String("<symbol> &lt; </symbol>")
        or l=To_Unbounded_String("<symbol> &amp; </symbol>") loop
         m:=To_Unbounded_String(Get_Line(my_file));
         if  l=To_Unbounded_String("<symbol> + </symbol>") then
            pr_Term(my_newfile,m);
            Put_Line(my_newfile,"add");
         else if
              l=To_Unbounded_String("<symbol> * </symbol>") then
            pr_Term(my_newfile,m);
               Put_Line(my_newfile,"call Math.multiply 2");
         else if
                 l=To_Unbounded_String("<symbol> / </symbol>") then
                 pr_Term(my_newfile,m);
                  Put_Line(my_newfile,"call Math.divide 2");
         else if
                 l=To_Unbounded_String("<symbol> &gt; </symbol>") then
                 pr_Term(my_newfile,m);
                     Put_Line(my_newfile,"gt");

         else if  l=To_Unbounded_String("<symbol> - </symbol>") then
            pr_Term(my_newfile,m);
                        Put_Line(my_newfile,"sub");

         else if  l=To_Unbounded_String("<symbol> | </symbol>") then
            pr_Term(my_newfile,m);
                           Put_Line(my_newfile,"or");

          else if  l=To_Unbounded_String("<symbol> = </symbol>") then
            pr_Term(my_newfile,m);
                Put_Line(my_newfile,"eq");

         else if  l=To_Unbounded_String("<symbol> &lt; </symbol>") then
            pr_Term(my_newfile,m);
             Put_Line(my_newfile,"lt");
          else if  l=To_Unbounded_String("<symbol> &amp; </symbol>") then
            pr_Term(my_newfile,m);
         Put_Line(my_newfile,"and");
          end if; end if; end if; end if; end if; end if; end if; end if; end if;


        --  Put_Line(my_newfile,To_String(wo) & To_String(l));

          -- pr_Term(my_newfile,(m));
      --   pr_Term(my_newfile,To_Unbounded_String(l));

      end loop;

    --  Put_Line(my_newfile,To_String(wo) & "</expression>");
end pr_Expression;

procedure  pr_subroutineCall(my_newfile: File_Type;ll:String;m:String) is
   begin
            -- Put_Line((ll) & (m));

      f:=f_find(getValue(To_Unbounded_String(ll)));
      if f/=0 then

         --Put_Line(my_newfile,"push " & To_String(kind) & Integer'Image(num));

         l:=To_Unbounded_String(Get_Line(my_file)); --

         tt:=getValue((l));
            l:=To_Unbounded_String(Get_Line(my_file)); --(
         type1:=ttype;
          pr_ExpressionList(my_newfile);--put_line(To_String(tt));
         numsParam:=counterParam+1;
      Put_Line(my_newfile,"call "& To_String(type1)&"."& To_String(tt) &integer'Image(numsParam));
      else

          --Put_Line(ll);
        -- l:=To_Unbounded_String(Get_Line(my_file));
         functionCall1:=getValue(To_Unbounded_String(ll));
         functionCall:=To_Unbounded_String("");
         numsParam:=0;
         --l:=To_Unbounded_String(Get_Line(my_file));--)
         --Put_Line(m);
         if getValue(To_Unbounded_String(m))="(" then
            Put_Line(my_newfile,"push pointer 0");
            functionCall:=(className) &To_Unbounded_String(".")& (functionCall1);
            counterParam:=0;
            pr_ExpressionList(my_newfile);

            numsParam:=counterParam+1;
            --l:=To_Unbounded_String(Get_Line(my_file));--)

         else
           -- if l=To_Unbounded_String("<symbol> ( </symbol>") then

            l:=To_Unbounded_String(Get_Line(my_file));

            functionCall2:=getValue(l);
	    l:=To_Unbounded_String(Get_Line(my_file));--(
            pr_ExpressionList(my_newfile);

	    numsParam:=counterParam;
            l:=To_Unbounded_String(Get_Line(my_file));--;

            functionCall:=functionCall1 & "." & functionCall2 ;
         end if;
       Put_Line(my_newfile,"call "& To_String(functionCall) & Integer'Image(numsParam)); --call func + num of arguments.
      end if;

-- l:=To_Unbounded_String(Get_Line(my_file));
       --   Put_Line(my_newfile,To_String(wo) & "</SubroutineCall>");
      end  pr_subroutineCall;

procedure pr_parse (my_newfile : File_Type) is
   begin
      pr_Class(my_newfile);
   end pr_parse;

procedure pr_Class(my_newfile : File_Type) is
   begin
        methodScopeIndex:=1;
        argumentIndex:=0;
	varIndex:=0;
        functionKind:=0;
        classScopeIndex:=1;
	staticIndex:=0;
	fieldIndex:=0;
      l:=To_Unbounded_String(Get_Line(my_file)); --class
     -- Put_Line(To_String(l));
    l:=To_Unbounded_String(Get_Line(my_file)); --className
    className:=getValue(l);
     -- Put_Line(To_String(nameClass));
    l:=To_Unbounded_String(Get_Line(my_file)); --{
    pr_ClassVarDec(my_newfile);
    pr_SubroutineDec(my_newfile);
      l:=To_Unbounded_String(Get_Line(my_file));--}--
   end pr_Class;

procedure pr_ClassVarDec(my_newfile: File_Type) is
   begin

      l:=To_Unbounded_String(Get_Line(my_file));

      while (l=To_Unbounded_String("<keyword> field </keyword>") or l=To_Unbounded_String("<keyword> static </keyword>")) loop
         classScope(3,classScopeIndex):=getValue(l);--static | field
         if getValue(l)="static" then
            classScope(4,classScopeIndex):=To_Unbounded_String(Integer'Image(staticIndex)); --static
            staticIndex:=staticIndex +1;
         else
            classScope(4,classScopeIndex):=To_Unbounded_String(Integer'Image(fieldIndex)); --field
	    fieldIndex := fieldIndex +1;
         end if;

         ttype:=getValue(To_Unbounded_String(Get_Line(my_file))); --type
         classScope(2,classScopeIndex):=tType;
         Name:=getValue(To_Unbounded_String(Get_Line(my_file)));--varName
         classScope(1,classScopeIndex):=tType;
         classScopeIndex:=classScopeIndex+1;
      l:=To_Unbounded_String(Get_Line(my_file)); -- ,varName*
                 while (To_String(l)/="<symbol> ; </symbol>") loop
                    symb:=getValue(l); -- ,
                    Name:=getValue(To_Unbounded_String(Get_Line(my_file)));--varName
           classScope(2,classScopeIndex):=classScope(2,classScopeIndex-1);
            classScope(3,classScopeIndex):=classScope(3,classScopeIndex-1);
            ll:=classScope(4,classScopeIndex-1);
          --  put_line(To_String(ll));
            classScope(4,classScopeIndex):=To_Unbounded_String(integer'Image( Integer'Value(To_String(ll))+1));
            --put_line(To_String(classScope(4,classScopeIndex)));
            if classScope(3,classScopeIndex-1)="static" then
               staticIndex:=staticIndex+1;
            else
               fieldIndex:=fieldIndex+1;
            end if;
            classScope(1,classScopeIndex):=Name;
            classScopeIndex:=classScopeIndex+1;
            l:=To_Unbounded_String(Get_Line(my_file));


         end loop;
if l=To_Unbounded_String("<symbol> ; </symbol>") then
            l:=To_Unbounded_String(Get_Line(my_file));
            end if;

      end loop;
 --
    end pr_ClassVarDec;

procedure pr_SubroutineDec(my_newfile: File_Type) is

   begin
      while (l=To_Unbounded_String("<keyword> constructor </keyword>") or l=To_Unbounded_String("<keyword> function </keyword>") or l=To_Unbounded_String("<keyword> method </keyword>")) loop
         tType:=getValue(l); -- constructor | method | func
         if  tType="constructor" then
            counterIf:=0;
            counterWhile:=0;
            for j in 1..5 loop
                  for k in 1..50 loop
                     methodScope(j,k):=To_Unbounded_String("");
                  end loop;
               end loop;
         --   methodScope:=string_Display(1..5,1..50);
            methodScopeIndex:=1;
            argumentIndex:=0;
	    varIndex:=0;
            functionKind:=0;
            l:=To_Unbounded_String(Get_Line(my_file));-- void | type
            Name:=getValue(To_Unbounded_String(Get_Line(my_file)));
           -- Put_Line(To_String(Name));
            tempWord:="function " & className &"."& Name & " ";
            l:=To_Unbounded_String(Get_Line(my_file));--(
            pr_ParameterList(my_newfile);
            pr_SubroutineBody(my_newfile);

         else if  tType="function" then
            counterIf:=0;
            counterWhile:=0;
               for j in 1..5 loop
                  for k in 1..50 loop
                     methodScope(j,k):=To_Unbounded_String("");
                  end loop;
               end loop;


          --  methodScope:= new string_Display(1..5,1..50);
            methodScopeIndex:=1;
            argumentIndex:=0;
	    varIndex:=0;
            functionKind:=1;
            l:=To_Unbounded_String(Get_Line(my_file));-- void | type
            Name:=getValue(To_Unbounded_String(Get_Line(my_file)));
            tempWord:="function " & className &"."& Name;
            l:=To_Unbounded_String(Get_Line(my_file));--(
            pr_ParameterList(my_newfile);
            pr_SubroutineBody(my_newfile);
         else -- method
            counterIf:=0;
            counterWhile:=0;
               functionKind:=2;
               methodScopeIndex:=1;
               argumentIndex:=0;
	      varIndex:=0;
            l:=To_Unbounded_String(Get_Line(my_file));-- void | type
            Name:=getValue(To_Unbounded_String(Get_Line(my_file)));
               tempWord:="function " & className &"."& Name;
               l:=To_Unbounded_String(Get_Line(my_file));--(
               for j in 1..5 loop
                  for k in 1..50 loop
                     methodScope(j,k):=To_Unbounded_String("");
                  end loop;
               end loop;
            --   methodScope:= new string_Display(1..5,1..50);

            pr_ParameterList(my_newfile);
            pr_SubroutineBody(my_newfile);
         end if; end if;
   --   l:=To_Unbounded_String(Get_Line(my_file));
         end loop;
      end pr_SubroutineDec;

procedure pr_ParameterList(my_newfile: File_Type) is
      begin
         if (functionKind = 2) then --method
           methodScope(1,methodScopeIndex):=To_Unbounded_String("this");
           methodScope(2,methodScopeIndex):=className;
           methodScope(3,methodScopeIndex):=To_Unbounded_String("argument");
           methodScope(4,methodScopeIndex):=To_Unbounded_String(Integer'Image(argumentIndex));
     -- Put_Line("I pp "& Integer'Image(methodScopeIndex) &"    " & argumentIndex'Image);


         argumentIndex := argumentIndex +1 ;
         methodScopeIndex := methodScopeIndex +1;
              --    Put_Line("h  " & Integer'Image(methodScopeIndex));

	 end if;

      l:=To_Unbounded_String(Get_Line(my_file));
     if (l/=To_Unbounded_String("<symbol> ) </symbol>")) then
         tType:=getValue(l); --type
         methodScope(2,methodScopeIndex):=tType;
         Name:=getValue(To_Unbounded_String(Get_Line(my_file))); --varName
         methodScope(1,methodScopeIndex):=Name;
         methodScope(3,methodScopeIndex):=To_Unbounded_String("argument");
         methodScope(4,methodScopeIndex):=To_Unbounded_String(Integer'Image(argumentIndex));

         argumentIndex :=argumentIndex +1 ;
   	 methodScopeIndex := methodScopeIndex +1;
         l:=To_Unbounded_String(Get_Line(my_file));
   end if;

      while To_String(l)/="<symbol> ) </symbol>" loop
        methodScope(2,methodScopeIndex):=tType;
         Name:=getValue(To_Unbounded_String(Get_Line(my_file))); --varName
         methodScope(1,methodScopeIndex):=Name;
         methodScope(3,methodScopeIndex):=To_Unbounded_String("argument");
         methodScope(4,methodScopeIndex):=To_Unbounded_String(Integer'Image(argumentIndex));

         argumentIndex := argumentIndex +1 ;
   	 methodScopeIndex := methodScopeIndex +1;
         l:=To_Unbounded_String(Get_Line(my_file));

      end loop;
    -- Put_Line(my_newfile,To_String(wo) & To_String(l));--)

    end pr_ParameterList;

procedure pr_SubroutineBody(my_newfile: File_Type) is
   begin
        l:=To_Unbounded_String(Get_Line(my_file)); --{

     -- Put_Line(To_String(tempWord) & "  " & To_String(tType));
      pr_VarDec(my_newfile);
      Put_Line(my_newfile,"// " & To_String(tType));
      Put_Line(my_newfile,To_String(tempWord) & Integer'Image(varIndex));
      if(functionKind = 0) then --constractor
        Put_Line(my_newfile,"push constant "& Integer'Image(fieldIndex)) ;--field'snum in class
        Put_Line(my_newfile,"call Memory.alloc 1"); --we sent 1 arg
        Put_Line(my_newfile,"pop pointer 0"); --lehaktzot my obj
      else if(functionKind = 2) then--method
        Put_Line(my_newfile,"push argument 0");--the first arg
        Put_Line(my_newfile,"pop pointer 0") ;--save THIS
         end if; end if;
     -- put_Line(To_String(l));
      pr_Statements(my_newfile);

             -- Put_Line(my_newfile,Get_Line(my_file); --}
     -- Put_Line(my_newfile,To_String(wo) &"<symbol> } </symbol>"); --}

        --  l:=To_Unbounded_String(Get_Line(my_file));


   end pr_SubroutineBody;


procedure  pr_VarDec(my_newfile:File_Type) is
   begin
      l:=To_Unbounded_String(Get_Line(my_file));
            while (l=To_Unbounded_String("<keyword> var </keyword>"))  loop
               methodScope(3,methodScopeIndex):=getValue(l); --var
               methodScope(2,methodScopeIndex):=getValue(To_Unbounded_String(Get_Line(my_file)));--type
               methodScope(1,methodScopeIndex):=getValue(To_Unbounded_String(Get_Line(my_file)));--Name
               methodScope(4,methodScopeIndex):=To_Unbounded_String(Integer'Image(varIndex));
         varIndex:=varIndex+1;
         methodScopeIndex:=methodScopeIndex+1;

       --  Put_Line(my_newfile,To_String(wo) & "<varDec>");
              --  Put_Line(my_newfile,To_String(wo) & To_String(l)); --var
               -- Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --type
               -- Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --varName
                l:=To_Unbounded_String(Get_Line(my_file)); -- ,varName*
         while To_String(l)/="<symbol> ; </symbol>" loop
            methodScope(3,methodScopeIndex):=methodScope(3,methodScopeIndex-1);--var
            methodScope(2,methodScopeIndex):=methodScope(2,methodScopeIndex-1);--type
            methodScope(1,methodScopeIndex):=getValue(To_Unbounded_String(Get_Line(my_file)));--Name
            methodScope(4,methodScopeIndex):=To_Unbounded_String(Integer'Image(varIndex));
            varIndex:=varIndex+1;
            methodScopeIndex:=methodScopeIndex+1;
            l:=To_Unbounded_String(Get_Line(my_file));
                end loop;
       --  Put_Line(my_newfile,To_String(wo) & To_String(l)); --;
         --          Put_Line(my_newfile,To_String(wo) & "</varDec>");
        l:=To_Unbounded_String(Get_Line(my_file));
      end loop;



               end  pr_VarDec;

procedure pr_Statements(my_newfile: File_Type) is
   begin
pr_Statement(my_newfile);
--Put_Line(my_newfile,To_String(wo) & "</statements>");
end pr_Statements;

procedure pr_Statement(my_newfile: File_Type) is
   begin
while l=To_Unbounded_String("<keyword> let </keyword>") or l=To_Unbounded_String("<keyword> do </keyword>") or l=To_Unbounded_String("<keyword> if </keyword>") or l=To_Unbounded_String("<keyword> while </keyword>") or l=To_Unbounded_String("<keyword> return </keyword>") loop
         if l=To_Unbounded_String("<keyword> let </keyword>") then

      pr_LetStatement(my_newfile); l:=To_Unbounded_String(Get_Line(my_file));--Put_Line(To_String(l));
     else  if l=To_Unbounded_String("<keyword> do </keyword>") then
        pr_DoStatement(my_newfile); l:=To_Unbounded_String(Get_Line(my_file)); --Put_Line(To_String(l));
     else  if l=To_Unbounded_String("<keyword> return </keyword>") then
      pr_ReturnStatement(my_newfile); l:=To_Unbounded_String(Get_Line(my_file)); --Put_Line(To_String(l));
     else  if l=To_Unbounded_String("<keyword> while </keyword>") then
      pr_WhileStatement(my_newfile); l:=To_Unbounded_String(Get_Line(my_file));
     else  if l=To_Unbounded_String("<keyword> if </keyword>") then
      pr_IfStatement(my_newfile);
                     end if; end if; end if; end if; end if;


      end loop;
end pr_Statement;

procedure pr_LetStatement(my_newfile: File_Type) is
begin

      Put_Line(my_newfile,"//let");

      temp:=getValue(To_Unbounded_String(Get_Line(my_file)));--varName
      --Put_Line(To_String(temp));

      l:=To_Unbounded_String(Get_Line(my_file));
             --  Put_Line(To_String(l));

      enterLoop:=0;

      if (l=To_Unbounded_String("<symbol> [ </symbol>")) then

         enterLoop:=1;
         l:=To_Unbounded_String(Get_Line(my_file));

         pr_Expression(my_newfile,To_String(l));

        -- l:=To_Unbounded_String(Get_Line(my_file));--]
--else l:=To_Unbounded_String(Get_Line(my_file));
      end if;
      l:=To_Unbounded_String(Get_Line(my_file));


       if enterLoop=1 then


         f:=f_find(temp);

         Put_Line(my_newfile,"push " & To_String(kind) & integer'Image(num));
         Put_Line(my_newfile,"add");
         --Put_Line(To_String(l));

         --l:=To_Unbounded_String(Get_Line(my_file));
         --Put_Line("s" & " " &To_String(l));
         --Put_Line("s" & To_String(l));

         pr_Expression(my_newfile,To_String(l));

         Put_Line(my_newfile,"pop temp 0");
         Put_Line(my_newfile,"pop pointer 1");
         Put_Line(my_newfile,"push temp 0");
         Put_Line(my_newfile,"pop that 0");
      else
        -- l:=To_Unbounded_String(Get_Line(my_file));
         pr_Expression(my_newfile,To_String(l));

         f:=f_find(temp);
         --Put_Line( To_String(kind) &  integer'Image(num));
         Put_Line(my_newfile,"pop " & To_String(kind) &  integer'Image(num));
--Put_Line(integer'Image(num) & " " & To_String(l));

    end if;


      if l=To_Unbounded_String("<symbol> ) </symbol>")  or l=To_Unbounded_String("<symbol> ] </symbol>") then
         l:=To_Unbounded_String(Get_Line(my_file));
      end if;
        -- Put_Line(To_String(l)); --]
      if l/=To_Unbounded_String("<symbol> ; </symbol>") then-- or l=To_Unbounded_String("<symbol> ] </symbol>") then
         l:=To_Unbounded_String(Get_Line(my_file));
      end if;
        -- Put_Line("   " &To_String(l)); --]
     --(To_String(l));
end pr_LetStatement;

procedure pr_IfStatement(my_newfile: File_Type) is
   begin
          Put_Line(my_newfile,"//If");

      im:=To_Unbounded_String(Integer'Image(counterIf));
      counterIf:=counterIf+1;
      l:=To_Unbounded_String(Get_Line(my_file));--(
      pr_Expression( my_newfile);
      Put_Line(my_newfile,"if-goto IF_TRUE_LABEL_" &(To_String(im)));
      Put_Line(my_newfile,"goto IF_FALSE_LABEL_" &(To_String(im) ));

      l:=To_Unbounded_String(Get_Line(my_file));--)
      l:=To_Unbounded_String(Get_Line(my_file));--{

      Put_Line(my_newfile,"label IF_TRUE_LABEL_" &(To_String(im)));

      pr_Statements(my_newfile);
     -- Put_Line(To_String(l));
      Put_Line(my_newfile,"goto IF_END_LABEL_" &(To_String(im)));
      Put_Line(my_newfile,"label IF_FALSE_LABEL_" & (To_String(im)));
      --Put_Line(my_newfile,To_String(wo) & To_String(l)); --}
      l:=To_Unbounded_String(Get_Line(my_file));--}

      if l=To_Unbounded_String("<keyword> else </keyword>") then

       l:=To_Unbounded_String(Get_Line(my_file)); --{
       --l:=To_Unbounded_String(Get_Line(my_file));

         pr_Statements(my_newfile);
         -- l:=To_Unbounded_String(Get_Line(my_file));--}
      counterIf:=counterIf-1;
   end if;

   Put_Line(my_newfile,"label IF_END_LABEL_" & To_String(im));

--Put_Line(my_newfile,To_String(wo) & "</ifStatement>");
end pr_IfStatement;

procedure pr_WhileStatement(my_newfile: File_Type) is
   begin
      Put_Line(my_newfile,"//while");
      im:= To_Unbounded_String(trim(Integer'Image(counterWhile),Both));

      counterWhile:=counterWhile+1;
      --Put_Line(my_newfile,To_String(wo) & To_String(l)); --while
      Put_Line(my_newfile,"label WHILE_START_LABEL_" &(To_String(im)));

      l:=To_Unbounded_String(Get_Line(my_file)); --(
      l:=To_Unbounded_String(Get_Line(my_file)); --
     -- Put_Line(To_String(l));
      pr_Expression(my_newfile,To_String(l));
      Put_Line(my_newfile,"not");
      Put_Line(my_newfile,"if-goto WHILE_END_LABEL_" &(To_String(im)));

      l:=To_Unbounded_String(Get_Line(my_file)); --{
            l:=To_Unbounded_String(Get_Line(my_file)); --state
      pr_Statements(my_newfile);
      --}

--Put_Line(my_newfile,To_String(wo) & "</whileStatement>");
      Put_Line(my_newfile,"goto WHILE_START_LABEL_" &(To_String(im)));
      Put_Line(my_newfile,"label WHILE_END_LABEL_" &(To_String(im)));


end pr_WhileStatement;

procedure pr_DoStatement(my_newfile: File_Type) is
   begin
      Put_Line(my_newfile,"//do");
      l := To_Unbounded_String(Get_Line(my_file));
      m := To_Unbounded_String(Get_Line(my_file));
      pr_subroutineCall(my_newfile,To_String(l),To_String(m));
      if l/=To_Unbounded_String("<symbol> ; </symbol>") and l/=To_Unbounded_String("<symbol> } </symbol>")  then
      l:=To_Unbounded_String(Get_Line(my_file));--;
            --Put_Line(To_String(l));
     end if;
      if l=To_Unbounded_String("<symbol> ) </symbol>") then
               l:=To_Unbounded_String(Get_Line(my_file));--;
      end if;
    --  Put_Line(To_String(l));
      Put_Line(my_newfile,"pop temp 0");

--Put_Line("l" &To_String(l));


end pr_DoStatement;

procedure pr_ReturnStatement(my_newfile: File_Type) is
   begin
 Put_Line(my_newfile,"//return");

 l:=To_Unbounded_String(Get_Line(my_file));

if (l/=To_Unbounded_String("<symbol> ; </symbol>")) then

  pr_Expression(my_newfile,To_String(l));
-- Put_Line(my_newfile,To_String(wo) & To_String(l));
         Put_Line(my_newfile,"return");
         l:=To_Unbounded_String(Get_Line(my_file));
         l:=To_Unbounded_String(Get_Line(my_file));


 --Put_Line(To_String(l));
      else
         --Put_Line(my_newfile,To_String(wo) & To_String(l)); --;
      Put_Line(my_newfile,"push constant 0");
         Put_Line(my_newfile,"return");
        l:=To_Unbounded_String(Get_Line(my_file));


      end if;

               end pr_ReturnStatement;

procedure pr_ExpressionList(my_newfile: File_Type) is
   begin
      counterParam:=0;
      l:=To_Unbounded_String(Get_Line(my_file));
      --Put_Line(To_String(l));
      if To_String(l)/=To_Unbounded_String("<symbol> ) </symbol>") then
         counterParam := counterParam+1;
       --  --Put_Line(To_String(l));
        -- if index(To_String(l),"<identifier>",1) /=0 then
       --put_Line(my_newfile,"push "&To_String(kind)& Integer'Image(num));
      --else
       --  Put_Line(my_newfile,"push this 0");
end if;
         pr_Expression(my_newfile,To_String(l));
--end if;
       --  l:=To_Unbounded_String(Get_Line(my_file));
         while (l=To_Unbounded_String("<symbol> , </symbol>")) loop
            l:=To_Unbounded_String(Get_Line(my_file));

                   counterParam := counterParam+1;
           pr_Expression(my_newfile,To_String(l));
          --  if l/=To_Unbounded_String("<symbol> ) </symbol>") then
          -- c
           --end if;
         end loop;
-- l:=To_Unbounded_String(Get_Line(my_file));
        --end if;
       --Put_Line(my_newfile,To_String(wo) & "</expressionList>");
      end pr_ExpressionList;



  procedure pr_Term(my_newfile: File_Type; ll:Unbounded_String) is
   begin

   if ll= To_Unbounded_String("") or ll=To_Unbounded_String("<symbol> = </symbol>")  then
         m:=To_Unbounded_String(Get_Line(my_file));
         else m:=ll;
      end if;

   if m=To_Unbounded_String("<symbol> ( </symbol>") then

         l:=To_Unbounded_String(Get_Line(my_file));
      pr_Expression(my_newfile,To_String(l));
         --Put_Line(my_newfile,To_String(wo) & To_String(l)); -- )
                        --       Put_Line(To_String(l)); -- (
              l:=To_Unbounded_String(Get_Line(my_file));


      else if m=To_Unbounded_String("<symbol> - </symbol>") or m=To_Unbounded_String("<symbol> ~ </symbol>") then
                  op:=getValue(m) ; -- -|~
                   l:=To_Unbounded_String(Get_Line(my_file));
                  pr_Term(my_newfile,l);
                  if(op="-") then
				Put_Line(my_newfile,"neg");
			else
				Put_Line(my_newfile,"not");
                  end if;





                else if index(To_String(m),"<integerConstant>",1) /=0 then
                t:=getValue(m);
        l:=To_Unbounded_String(Get_Line(my_file));
                  Put_Line(my_newfile,"push constant "& To_String(t));

                   else if index(To_String(m),"<stringConstant>",1) /=0 then
                     --  t:=getValue(m);--id
                     t:=To_Unbounded_String(Head(Tail(To_String(m),Length(m)-17),Length(m)-35));
                    -- Put_Line(To_String(t));
        lent:=Length(t);
        Put_Line(my_newfile,"push constant "&Integer'Image(lent));
         Put_Line(my_newfile,"call String.new 1");
                     for i in 1..Lent loop
                     --   if Element(t,i)/=' ' then
                           --Put_Line(Element(t,i));
                          -- Put_Line(To_String(m));
		           Put_Line(my_newfile,"push constant "& Integer'Image(Character'Pos(Element(t,i))));
                           put_line(my_newfile,"call String.appendChar 2");
                        -- l:=To_Unbounded_String(Get_Line(my_file));
	end loop;
               else if index(To_String(m),"<keyword>",1) /=0 then

               if To_String(getValue(m))= "null" or  To_String(getValue(m))= "false" then
				Put_Line(my_newfile,"push constant 0");
			else if To_String(getValue(m))= "true" then
				put_line(my_newfile,"push constant 0");
				Put_Line(my_newfile,"not");
			else if To_String(getValue(m))= "this" then
				Put_Line(my_newfile,"push pointer 0");


                        else --if index(To_String(m),"<keyword>",1) /=0 then
                              t := getValue(m); --id
                              f:=f_find(t);
                              Put_Line(my_newfile,"push "&  To_String(kind) &Integer'Image(num));
                           end if;
                        end if;
                       -- l:=To_Unbounded_String(Get_Line(my_file));
                        end if;

        else if index(To_String(m),"<identifier>",1) /=0 then
                         -- Put_Line("ooood" &To_String(m));

                           l:=To_Unbounded_String(Get_Line(my_file));

                   if l=To_Unbounded_String("<symbol> [ </symbol>") then
                              n:=getValue(m);--varName
                              l:=To_Unbounded_String(Get_Line(my_file));
                              --Put_Line(my_newfile,To_String(getValue(l)));
                              pr_Expression(my_newfile,To_String(l));
                              f:=f_find(n);
                             Put_Line(my_newfile,"push "& To_String(kind)&  Integer'Image(num));
			Put_Line(my_newfile,"add");
			Put_Line(my_newfile,"pop pointer 1");
                        Put_Line(my_newfile,"push that 0");



                  else if l=To_Unbounded_String("<symbol> . </symbol>") or l=To_Unbounded_String("<symbol> ( </symbol>") then
                               --  l:=To_Unbounded_String(Get_Line(my_file));
                               --  Put_Line(To_String(m));
                                 pr_subroutineCall(my_newfile,To_String(m),To_String(l));
  else
                         t := getValue(m);
                                 f:=f_find(t);
                           Put_Line(my_newfile,"push "& To_String(kind)& Integer'Image(num));
                              end if; end if;

                     else
                         t := getValue(m);--Put_Line(To_String(m) & " " & To_String(l));
                        f:=f_find(t);
                        if  To_String(kind)="this" then
                           Put_Line(my_newfile,"push "& To_String(kind)& Integer'Image(num));
                           end if;

--                          Put_Line(my_newfile,"push this 0");


                        --   l:=To_Unbounded_String(Get_Line(my_file));

                           --     Put_Line(my_newfile,To_String(wo) & To_String(m));--id
                       -- else

                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;     --end if; --end if; end if;
end pr_Term;



   begin
    Open(d_file,In_File,dd);
      while not End_Of_File(d_file)
   loop
      l:=To_Unbounded_String(get_Line(d_file));
      -- Put_Line(To_String(l));
      className:=To_Unbounded_String(get_Line(d_file));
     -- Put_Line(To_String(className));
   Open(my_file, In_File,To_String(l));
      Create(my_newfile, out_File, head(To_String(l),Length((l))-5) & ".vm");
     l:=To_Unbounded_String(Get_Line(my_file));
      pr_parse(my_newfile);
   --   for j in 1..4 loop
     --   for i in 1..4 loop
       --     Put(To_String(classScope(i,j)) &"  ");
         --       end loop;
           --     Put_Line(" ");
             --   end loop;

   Close(my_file);
   Close(my_newfile);

   end loop;
   Close(d_file);

end Main;
