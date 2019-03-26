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
   m,l: Unbounded_String;
   w: Unbounded_String;
   b: Character:='j';
   c: Character;
   word,word3,word2:Unbounded_String;

   dd:String:= "C:\AAA EKRONOT\Targil 4\dir.txt";

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

   function oneword(my_file : File_Type; c: out Character) return Unbounded_String is
   begin
       w:=To_Unbounded_String("");
       Get(my_file,c);
      while c/=' ' loop

         w:=w&c;
         if not End_Of_Line(my_file) then
            Get(my_file,c);
         else
            c:=' ';
         end if;
         end loop;
return w;
      end oneword;

   procedure pr_token(my_newfile : File_Type;word,s:String) is
            begin
            Put(my_newfile,"<" & s & "> ");
            Put(my_newfile, word);
            Put_Line(my_newfile," </" & s & ">");
            end pr_token;


begin
    Open(d_file,In_File,dd);
      while not End_Of_File(d_file)
   loop
      l:=To_Unbounded_String(get_Line(d_file));
      m:=To_Unbounded_String(get_Line(d_file));
              word:=To_Unbounded_String("");

   Open(my_file, In_File,To_String(l));
   Create(my_newfile, out_File, head(To_String(l),Length((l))-5) & "T.xml");
Put_Line(my_newfile,"<tokens>");
   while not End_Of_File(my_file)
      loop
         get(my_file,b);

         if b='/' then get(my_file,b);
            if b='/' then l:=To_Unbounded_String(Get_Line(my_file));
            else if b='*' then get(my_file,c);
                  while (b&c /= "*/") loop
                     b:=c;
                     get(my_file,c);
                  end loop;
                  b:=c;
               else word:=To_Unbounded_String("/");
                  pr_token(my_newfile,To_String(word),"symbol");

               end if;
            end if;
         word:=To_Unbounded_String("");
         end if;

         if b=' '  then put(":)");--get(my_file,b);



           end if;
            if b='"' then
                get(my_file,b);
            while b/='"' loop
               word:=word & b;
               get(my_file,b);
                end loop;
             pr_token(my_newfile,To_String(word),"stringConstant");
             word:=To_Unbounded_String("");
         end if;

         if Is_Letter(b) then
                  word:=b & word;
                  get(my_file,b);
               while (Is_Digit(b) or Is_Letter(b) or b='_') loop
                     word:=word &b;
                     get(my_file,b);
                  end loop;
               if f_keyword(To_String(word))="No" then
                  pr_token(my_newfile,To_String(word),"identifier");
               else pr_token(my_newfile,To_String(word),"keyword");
                  end if;
           word:=To_Unbounded_String("");
         end if;

         if Is_Digit(b) then
                  word:=b & word;
                  get(my_file,b);
               while (Is_Digit(b) ) loop
                     word:=word &b;
                     get(my_file,b);
                     end loop;
                     pr_token(my_newfile,To_String(word),"integerConstant");

        word:=To_Unbounded_String("");
        end if;



             if f_symbol(b)/='!' then
            word:=word & b;
            if b='<' then word := To_Unbounded_String("&lt;"); end if;
            if b='>' then word := To_Unbounded_String("&gt;"); end if;
            if b='&' then word := To_Unbounded_String("&amp;");end if;
            pr_token(my_newfile,To_String(word),"symbol");
            word:=To_Unbounded_String("");

         end if;


   end loop;
Put(my_newfile,"</tokens>");
   Close(my_file);
   Close(my_newfile);

   end loop;
   Close(d_file);

end Main;
