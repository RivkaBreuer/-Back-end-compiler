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


procedure Main is

 package SU renames ada.Strings.Unbounded;
 package D renames ada.Directories;

 my_file, my_newfile, d_file : File_Type;
   m,ll,l: Unbounded_String;
   w,wo: Unbounded_String;
   b: Character:='j';
   word1,word3,word2:Unbounded_String;
   i: Integer:=0;
   offset: String(1..1):=" ";
   woffset: Unbounded_String;

   dd:String:= "C:\AAA EKRONOT\Targil 4 b\dir.txt";

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

   function f_offset(i:Integer) return Unbounded_String is
   begin
      w:=To_Unbounded_String("");
         for j in 1..i loop
         w:=w & woffset;
           end loop;
      return w;
               end f_offset;

   procedure pr_Expression(my_newfile: File_Type; sl:String:="") is

   begin
      Put_Line(my_newfile,To_String(wo) & "<expression>");
      i:=i+1;
      wo:=f_offset(i);
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
          Put_Line(my_newfile,To_String(wo) & To_String(l));
           m:=To_Unbounded_String(Get_Line(my_file));
           pr_Term(my_newfile,(m));
      --   pr_Term(my_newfile,To_Unbounded_String(l));

      end loop;
      i:=i-1;
      wo:=f_offset(i);
      Put_Line(my_newfile,To_String(wo) & "</expression>");
end pr_Expression;

    procedure  pr_subroutineCall(my_newfile: File_Type;l:String:="";m:String:="") is
   begin
      Put_Line(my_newfile,To_String(wo) & "<SubroutineCall>");
       i:=i+1;
      wo:=f_offset(i);
      Put_Line(my_newfile,To_String(wo) & (l)); --id
      if m=To_Unbounded_String("<symbol> ( </symbol>") then
      Put_Line(my_newfile,To_String(wo) & (m)); --(
         pr_ExpressionList(my_newfile);
         put_Line(my_newfile,To_String(wo) & (l));
      else
         Put_Line(my_newfile,To_String(wo) & (m)); --.
         put_Line(my_newfile,To_String(wo) & Get_Line(my_file));--id
         put_Line(my_newfile,To_String(wo) & Get_Line(my_file));--(
         pr_ExpressionList(my_newfile);
         put_Line(my_newfile,To_String(wo) & (l));--)

         end if;
      i:=i-1;
      wo:=f_offset(i);
          Put_Line(my_newfile,To_String(wo) & "</SubroutineCall>");
   end  pr_subroutineCall;


procedure pr_parse (my_newfile : File_Type) is
      begin
      pr_Class(my_newfile);
   end pr_parse;

procedure pr_Class(my_newfile : File_Type) is
   begin
      i:=i+1;
      wo:=f_offset(i);
      Put_Line(my_newfile,"<class>");
    --  put(my_newfile,To_String(wo));
    Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --class

    Put_Line(my_newfile,To_String(wo) &Get_Line(my_file)); --className
    Put_Line(my_newfile,To_String(wo) &Get_Line(my_file)); --{
    pr_ClassVarDec(my_newfile);
    pr_SubroutineDec(my_newfile);
    Put_Line(my_newfile,To_String(wo) &To_String(l));--}
    i:=i-1;
      Put_Line(my_newfile,"</class>");
   end pr_Class;

procedure pr_ClassVarDec(my_newfile: File_Type) is
   begin

      l:=To_Unbounded_String(Get_Line(my_file));
      while (l=To_Unbounded_String("<keyword> field </keyword>") or l=To_Unbounded_String("<keyword> static </keyword>")) loop
         Put_Line(my_newfile,To_String(wo) & "<classVarDec>");
          i:=i+1;
         wo:=f_offset(i);
                 Put_Line(my_newfile,To_String(wo) & To_String(l)); --static | field
                 Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --type
                 Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --varName
                 l:=To_Unbounded_String(Get_Line(my_file)); -- ,varName*
                 while (To_String(l)/="<symbol> ; </symbol>") loop
                     Put_Line(my_newfile,To_String(wo) & To_String(l)); -- ,
                      Put_Line(my_newfile,To_String(wo) & Get_Line(my_file));
                     l:=To_Unbounded_String(Get_Line(my_file));

         end loop;
         Put_Line(my_newfile,To_String(wo) & To_String(l)); --;
         i:=i-1;
         wo:=f_offset(i);

         Put_Line(my_newfile,To_String(woffset) &"</classVarDec>");
          l:=To_Unbounded_String(Get_Line(my_file));
      end loop;

    end pr_ClassVarDec;

procedure pr_SubroutineDec(my_newfile: File_Type) is
   begin
      while (l=To_Unbounded_String("<keyword> constructor </keyword>") or l=To_Unbounded_String("<keyword> function </keyword>") or l=To_Unbounded_String("<keyword> method </keyword>")) loop
         Put_Line(my_newfile,To_String(wo) & "<subroutineDec>");
          i:=i+1;
      wo:=f_offset(i);
                Put_Line(my_newfile,To_String(wo) & To_String(l)); -- constructor | method | func
                Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); -- void | type
                Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); -- subrutinoName
                Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); -- (
                pr_ParameterList(my_newfile);
              --  Put_Line(my_newfile,Get_Line(my_file)); -- )
         pr_SubroutineBody(my_newfile);
          i:=i-1;
      wo:=f_offset(i);
         Put_Line(my_newfile,To_String(wo) & "</subroutineDec>");
         l:=To_Unbounded_String(Get_Line(my_file));
         end loop;
      end pr_SubroutineDec;

procedure pr_ParameterList(my_newfile: File_Type) is
       begin
     Put_Line(my_newfile,To_String(wo) & "<parameterList>");
      l:=To_Unbounded_String(Get_Line(my_file));
      i:=i+1;
      wo:=f_offset(i);
     if (l/=To_Unbounded_String("<symbol> ) </symbol>")) then
     Put_Line(my_newfile,To_String(wo) &  To_String(l)); --type
        Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --varName
         l:=To_Unbounded_String(Get_Line(my_file));
   end if;

      while To_String(l)/="<symbol> ) </symbol>" loop
       Put_Line(my_newfile,To_String(wo) & To_String(l)); --,
       Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --type
         Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --varName
          l:=To_Unbounded_String(Get_Line(my_file));
      end loop;
i:=i-1;
      wo:=f_offset(i);
     Put_Line(my_newfile,To_String(wo) & "</parameterList>");
     Put_Line(my_newfile,To_String(wo) & To_String(l));--)

    end pr_ParameterList;

procedure pr_SubroutineBody(my_newfile: File_Type) is
   begin

      Put_Line(my_newfile,To_String(wo) & "<subroutineBody>");
      i:=i+1;
      wo:=f_offset(i);
    Put_Line(my_newfile,To_String(wo)& Get_Line(my_file)); --{
             pr_VarDec(my_newfile);
             pr_Statements(my_newfile);
             -- Put_Line(my_newfile,Get_Line(my_file); --}
      Put_Line(my_newfile,To_String(wo) &"<symbol> } </symbol>"); --}
      i:=i-1;
      wo:=f_offset(i);
      Put_Line(my_newfile,To_String(wo) & "</subroutineBody>");
        --  l:=To_Unbounded_String(Get_Line(my_file));


   end pr_SubroutineBody;

procedure  pr_VarDec(my_newfile:File_Type) is
   begin
      l:=To_Unbounded_String(Get_Line(my_file));
            while (l=To_Unbounded_String("<keyword> var </keyword>"))  loop

         Put_Line(my_newfile,To_String(wo) & "<varDec>");
         i:=i+1;
         wo:=f_offset(i);
                Put_Line(my_newfile,To_String(wo) & To_String(l)); --var
                Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --type
                Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --varName
                l:=To_Unbounded_String(Get_Line(my_file)); -- ,varName*
                while To_String(l)/="<symbol> ; </symbol>" loop
                  Put_Line(my_newfile,To_String(wo) & To_String(l)); -- ,
                    Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); -- varName
                    l:=To_Unbounded_String(Get_Line(my_file));
                 end loop;
         Put_Line(my_newfile,To_String(wo) & To_String(l)); --;
         i:=i-1;
         wo:=f_offset(i);
                   Put_Line(my_newfile,To_String(wo) & "</varDec>");
                   l:=To_Unbounded_String(Get_Line(my_file));
                   end loop;
               end  pr_VarDec;

procedure pr_Statements(my_newfile: File_Type) is
   begin
Put_Line(my_newfile,To_String(wo) & "<statements>");
pr_Statement(my_newfile);
Put_Line(my_newfile,To_String(wo) & "</statements>");
end pr_Statements;

procedure pr_Statement(my_newfile: File_Type) is
   begin
    i:=i+1;
    wo:=f_offset(i);
--Put_Line(my_newfile,"<statement>");
--l:=To_Unbounded_String(Get_Line(my_file)); -- if | while | do | return |let
while l=To_Unbounded_String("<keyword> let </keyword>") or l=To_Unbounded_String("<keyword> do </keyword>") or l=To_Unbounded_String("<keyword> if </keyword>") or l=To_Unbounded_String("<keyword> while </keyword>") or l=To_Unbounded_String("<keyword> return </keyword>") loop
    if l=To_Unbounded_String("<keyword> let </keyword>") then
      pr_LetStatement(my_newfile); l:=To_Unbounded_String(Get_Line(my_file));
     else  if l=To_Unbounded_String("<keyword> do </keyword>") then
        pr_DoStatement(my_newfile); l:=To_Unbounded_String(Get_Line(my_file));
     else  if l=To_Unbounded_String("<keyword> return </keyword>") then
      pr_ReturnStatement(my_newfile); l:=To_Unbounded_String(Get_Line(my_file));
     else  if l=To_Unbounded_String("<keyword> while </keyword>") then
      pr_WhileStatement(my_newfile); l:=To_Unbounded_String(Get_Line(my_file));
     else  if l=To_Unbounded_String("<keyword> if </keyword>") then
      pr_IfStatement(my_newfile);
                     end if; end if; end if; end if; end if;


      end loop;
 i:=i-1;
 wo:=f_offset(i);
--Put_Line(my_newfile,"</statement>");
end pr_Statement;

procedure pr_LetStatement(my_newfile: File_Type) is
begin

      Put_Line(my_newfile,To_String(wo) & "<letStatement>");
      i:=i+1;
wo:=f_offset(i);
Put_Line(my_newfile,To_String(wo) & To_String(l)); --let
Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --varName
           l:=To_Unbounded_String(Get_Line(my_file));

if (l/=To_Unbounded_String("<symbol> = </symbol>")) then
         Put_Line(my_newfile,To_String(wo) & To_String(l)); --[
                 --   l:=To_Unbounded_String(Get_Line(my_file));
  pr_Expression(my_newfile);
         Put_Line(my_newfile,To_String(wo) & To_String(l)); --]
         l:=To_Unbounded_String(Get_Line(my_file));

               end if;
      Put_Line(my_newfile,To_String(wo) & To_String(l)); --=
           --      l:=To_Unbounded_String(Get_Line(my_file));

pr_Expression(my_newfile);
Put_Line(my_newfile,To_String(wo) & To_String(l)); --;
      i:=i-1;
      wo:=f_offset(i);
Put_Line(my_newfile,To_String(wo) & "</letStatement>");

end pr_LetStatement;

procedure pr_IfStatement(my_newfile: File_Type) is
   begin
      Put_Line(my_newfile,To_String(wo) & "<ifStatement>");
      i:=i+1;
      wo:=f_offset(i);
      Put_Line(my_newfile,To_String(wo) & To_String(l)); --if
      Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --(
     -- l:=To_Unbounded_String(Get_Line(my_file));
      pr_Expression( my_newfile);
      Put_Line(my_newfile,To_String(wo) & To_String(l)); --)
      Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --{
      l:=To_Unbounded_String(Get_Line(my_file));
      pr_Statements(my_newfile);
      Put_Line(my_newfile,To_String(wo) & To_String(l)); --}
      l:=To_Unbounded_String(Get_Line(my_file));
      if l=To_Unbounded_String("<keyword> else </keyword>") then
         Put_Line(my_newfile,To_String(wo) & To_String(l)); --else
         Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --{
       l:=To_Unbounded_String(Get_Line(my_file));

       pr_Statements(my_newfile);
       Put_Line(my_newfile,To_String(wo) & To_String(l)); --}

        -- else l:=To_Unbounded_String(Get_Line(my_file));
   end if;

      i:=i-1;
      wo:=f_offset(i);

Put_Line(my_newfile,To_String(wo) & "</ifStatement>");
end pr_IfStatement;

procedure pr_WhileStatement(my_newfile: File_Type) is
   begin
      Put_Line(my_newfile,To_String(wo) & "<whileStatement>");
       i:=i+1;
      wo:=f_offset(i);
        Put_Line(my_newfile,To_String(wo) & To_String(l)); --while
      Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --(
       pr_Expression(my_newfile,"");
      Put_Line(my_newfile,To_String(wo) & To_String(l)); --)
      Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --{
      l:=To_Unbounded_String(Get_Line(my_file));
       pr_Statements(my_newfile);
      Put_Line(my_newfile,To_String(wo) & To_String(l)); --}
      i:=i-1;
      wo:=f_offset(i);
             Put_Line(my_newfile,To_String(wo) & "</whileStatement>");


end pr_WhileStatement;

procedure pr_DoStatement(my_newfile: File_Type) is
   begin
      Put_Line(my_newfile,To_String(wo) & "<doStatement>");
      i:=i+1;
      wo:=f_offset(i);
      Put_Line(my_newfile,To_String(wo) & To_String(l)); --do
      Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --id

      l:=To_Unbounded_String(Get_Line(my_file));
      Put_Line(my_newfile,To_String(wo) & To_String(l)); --. | (
      if l = To_Unbounded_String("<symbol> . </symbol>") then
         Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --id
           Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --(
         end if;
         pr_ExpressionList(my_newfile);
      Put_Line(my_newfile,To_String(wo) & To_String(l)); --)

        Put_Line(my_newfile,To_String(wo) & Get_Line(my_file)); --;
      i:=i-1;
      wo:=f_offset(i);
      Put_Line(my_newfile,To_String(wo) & "</doStatement>");


end pr_DoStatement;

procedure pr_ReturnStatement(my_newfile: File_Type) is
   begin
      Put_Line(my_newfile,To_String(wo) & "<returnStatement>");
      i:=i+1;
      wo:=f_offset(i);
      Put_Line(my_newfile,To_String(wo) & To_String(l)); --return
 l:=To_Unbounded_String(Get_Line(my_file));

if (l/=To_Unbounded_String("<symbol> ; </symbol>")) then

  pr_Expression(my_newfile,To_String(l));
 Put_Line(my_newfile,To_String(wo) & To_String(l));

     else Put_Line(my_newfile,To_String(wo) & To_String(l)); --;
      end if;
      i:=i-1;
      wo:=f_offset(i);
             Put_Line(my_newfile,To_String(wo) & "</returnStatement>");
               end pr_ReturnStatement;



   procedure pr_ExpressionList(my_newfile: File_Type) is
   begin
      Put_Line(my_newfile,To_String(wo) & "<expressionList>");
      i:=i+1;
      wo:=f_offset(i);
      l:=To_Unbounded_String(Get_Line(my_file));
      if To_String(l)/=To_Unbounded_String("<symbol> ) </symbol>") then
         pr_Expression(my_newfile,To_String(l));

       --  l:=To_Unbounded_String(Get_Line(my_file));
         while (l=To_Unbounded_String("<symbol> , </symbol>")) loop
            Put_Line(my_newfile,To_String(wo) & To_String(l));--,
            l:=To_Unbounded_String(Get_Line(my_file));
           pr_Expression(my_newfile,To_String(l));
          --  if l/=To_Unbounded_String("<symbol> ) </symbol>") then
          --  l:=To_Unbounded_String(Get_Line(my_file));
          -- end if;
         end loop;
         else put(":(");
        end if;
 i:=i-1;
    wo:=f_offset(i);
       Put_Line(my_newfile,To_String(wo) & "</expressionList>");
      end pr_ExpressionList;


   procedure pr_Term(my_newfile: File_Type; ll:Unbounded_String) is
      begin
      Put_Line(my_newfile,To_String(wo) & "<term>");
      i:=i+1;
      wo:=f_offset(i);
   if ll= To_Unbounded_String("") then
         m:=To_Unbounded_String(Get_Line(my_file));
         else m:=ll;
      end if;

   if m=To_Unbounded_String("<symbol> ( </symbol>") then
      Put_Line(my_newfile,To_String(wo) & To_String(m)); -- (
      pr_Expression(my_newfile);
         Put_Line(my_newfile,To_String(wo) & To_String(l)); -- )
                           l:=To_Unbounded_String(Get_Line(my_file));

      else if m=To_Unbounded_String("<symbol> - </symbol>") or m=To_Unbounded_String("<symbol> ~ </symbol>") then
      Put_Line(my_newfile,To_String(wo) & To_String(m)); -- -|~
         l:=To_Unbounded_String(Get_Line(my_file));
            pr_Term(my_newfile,l);
        --  l:=To_Unbounded_String(Get_Line(my_file));

      else if index(To_String(m),"<keyword>",1) /=0 then
               Put_Line(my_newfile,To_String(wo) & To_String(m));
               l:=To_Unbounded_String(Get_Line(my_file));

                else if index(To_String(m),"<integerConstant>",1) /=0 then
               Put_Line(my_newfile,To_String(wo) & To_String(m));
                  l:=To_Unbounded_String(Get_Line(my_file));

                   else if index(To_String(m),"<stringConstant>",1) /=0 then
               Put_Line(my_newfile,To_String(wo) & To_String(m));
                     l:=To_Unbounded_String(Get_Line(my_file));

                      else if index(To_String(m),"<keywordConstant>",1) /=0 then
               Put_Line(my_newfile,To_String(wo) & To_String(m));
                  l:=To_Unbounded_String(Get_Line(my_file));

      else if index(To_String(m),"<identifier>",1) /=0 then
                  l:=To_Unbounded_String(Get_Line(my_file));
                   if l=To_Unbounded_String("<symbol> [ </symbol>") then
            Put_Line(my_newfile,To_String(wo) & To_String(m));--varName
            Put_Line(my_newfile,To_String(wo) & To_String(l));--[
            pr_Expression(my_newfile);
                     Put_Line(my_newfile,To_String(wo) & To_String(l));--]
                                       l:=To_Unbounded_String(Get_Line(my_file));

                  else if l=To_Unbounded_String("<symbol> . </symbol>") or l=To_Unbounded_String("<symbol> ( </symbol>") then
                       Put_Line(my_newfile,To_String(wo) & To_String(m)); --id
      if l=To_Unbounded_String("<symbol> ( </symbol>") then
      Put_Line(my_newfile,To_String(wo) & To_String(l)); --(
         pr_ExpressionList(my_newfile);
         put_Line(my_newfile,To_String(wo) & To_String(l));
      else
         Put_Line(my_newfile,To_String(wo) & To_String(l)); --.
         put_Line(my_newfile,To_String(wo) & Get_Line(my_file));--id
         put_Line(my_newfile,To_String(wo) & Get_Line(my_file));--(
         pr_ExpressionList(my_newfile);
         put_Line(my_newfile,To_String(wo) & To_String(l));--)

         end if;
                       l:=To_Unbounded_String(Get_Line(my_file));
                     else


                 Put_Line(my_newfile,To_String(wo) & To_String(m));--id
 end if;              end if;
               end if;    end if;   end if; end if;     end if; end if; end if;
      i:=i-1;
      wo:=f_offset(i);
Put_Line(my_newfile,To_String(wo) & "</term>");
end pr_Term;


   begin
    Open(d_file,In_File,dd);
      while not End_Of_File(d_file)
   loop
      l:=To_Unbounded_String(get_Line(d_file));
      m:=To_Unbounded_String(get_Line(d_file));
     woffset:=To_Unbounded_String("  ");
   Open(my_file, In_File,To_String(l));
      Create(my_newfile, out_File, head(To_String(l),Length((l))-5) & ".xml");
     l:=To_Unbounded_String(Get_Line(my_file));
     i:=0;
     pr_parse(my_newfile);

   Close(my_file);
   Close(my_newfile);

   end loop;
   Close(d_file);

end Main;

