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

procedure Main is

 package SU renames ada.Strings.Unbounded;
 package D renames ada.Directories;

 my_file, my_newfile, d_file : File_Type;
   m,l: Unbounded_String;
   w: Unbounded_String;
   b: Character:='j';
   word,word3,word2:Unbounded_String;
   I: Integer :=0;
   C: Integer :=1;
   dir:Unbounded_String;
   s: String(1..11);

   dd:String:= "C:\AAA EKRONOT\r.txt";

   procedure  pr_push_c (my_newfile: File_Type; word3: String) is

   begin
               Put_Line(my_newfile,("//push constans " & word3));
               Put_Line(my_newfile,("@" & word3));
               Put_Line(my_newfile,"D=A");
               Put_Line(my_newfile,"@SP");
               Put_Line(my_newfile,"A=M");
               Put_Line(my_newfile,"M=D");
               Put_Line(my_newfile,"@SP");
               Put_Line(my_newfile,"M=M+1");

   end pr_push_c;

   procedure  pr_push (my_newfile: File_Type; word1,word3: String) is

   begin
               Put_Line(my_newfile,("//push "& word1 & " " & word3));
               Put_Line(my_newfile,("@" & word3));
               Put_Line(my_newfile,"D=A");
               Put_Line(my_newfile,"@" & word1);
               Put_Line(my_newfile,"A=M+D");
               Put_Line(my_newfile,"D=M");
               Put_Line(my_newfile,"@SP");
               Put_Line(my_newfile,"A=M");
               Put_Line(my_newfile,"M=D");
               Put_Line(my_newfile,"@SP");
               Put_Line(my_newfile,"M=M+1");

   end pr_push;

   procedure  pr_push_t (my_newfile: File_Type;  word3: String) is

   begin
      Put_Line(my_newfile,("//push temp "& word3));
      Put_Line(my_newfile,("@" & word3));
      Put_Line(my_newfile,"D=A");
      Put_Line(my_newfile,"A=D");
      for i in 1..5 loop
         Put_Line(my_newfile,"A=A+1");
      end loop;
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M");
      Put_Line(my_newfile,"M=D");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M+1");

   end pr_push_t;

   procedure  pr_push_p (my_newfile: File_Type;  word3: String) is

   begin
     Put_Line(my_newfile,("//push pointer "& word3));
     Put_Line(my_newfile,("@THIS"));
     Put_Line(my_newfile,"D=A");
     Put_Line(my_newfile,"@" & word3);
     Put_Line(my_newfile,"A=D+A");
     Put_Line(my_newfile,"D=M");
     Put_Line(my_newfile,"@SP");
     Put_Line(my_newfile,"A=M");
     Put_Line(my_newfile,"M=D");
     Put_Line(my_newfile,"@SP");
     Put_Line(my_newfile,"M=M+1");

   end pr_push_p;

   procedure  pr_push_st (my_newfile: File_Type;  word3: String) is

   begin
     Put_Line(my_newfile,("//push STATIC "& word3));
     Put_Line(my_newfile,("@"& To_String(m) & "." & word3));
     Put_Line(my_newfile,"D=M");
     Put_Line(my_newfile,"@SP");
     Put_Line(my_newfile,"A=M");
     Put_Line(my_newfile,"M=D");
     Put_Line(my_newfile,"@SP");
     Put_Line(my_newfile,"M=M+1");

   end pr_push_st;

   procedure  pr_pop_c (my_newfile: File_Type; word3: String) is
   begin
      Put_Line(my_newfile,("//pop constans "& word3));
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@" & word3);
      Put_Line(my_newfile,"A=M");
      for i in 1..Integer'Value(word3) loop
        Put_Line(my_newfile,"A=A+1");
      end loop;

      Put_Line(my_newfile,"M=D");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,("M=M-1"));

   end pr_pop_c;

   procedure  pr_pop (my_newfile: File_Type; word1,word3: String) is
      i:Integer:=0;
   begin
      Put_Line(my_newfile,("//pop "& word1 & " "  & word3));
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,("@" & word1));
      Put_Line(my_newfile,("A=M"));

      for i in 1..Integer'Value(word3) loop
      Put_Line(my_newfile,"A=A+1");
         end loop;
      Put_Line(my_newfile,"M=D");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,("M=M-1"));
   end pr_pop;

   procedure  pr_pop_t (my_newfile: File_Type; word3: String) is
   begin
                           Put_Line(my_newfile,("//pop temp " & word3));

      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,("@" & word3));
for i in 1..5 loop
         Put_Line(my_newfile,"A=A+1"); end loop;
         Put_Line(my_newfile,"M=D");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M-1");

   end pr_pop_t;

   procedure  pr_pop_p (my_newfile: File_Type; word3: String) is
   begin
      Put_Line(my_newfile,("//pop pointer " & word3));
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,("@THIS"));
       for i in 1..Integer'Value(word3) loop
      Put_Line(my_newfile,"A=A+1");
         end loop;
      Put_Line(my_newfile,"M=D");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M-1");

   end pr_pop_p;

   procedure  pr_pop_st (my_newfile: File_Type; word3: String) is
   begin
      Put_Line(my_newfile,("//pop STATIC " & word3));

      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M-1");
      Put_Line(my_newfile,"A=M");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@"& To_String(m) & "."  & word3);
      Put_Line(my_newfile,"M=D");


   end pr_pop_st;



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

   procedure  pr_add (my_newfile: File_Type) is
   begin
               Put_Line(my_newfile,("//add"));
               Put_Line(my_newfile,"@SP");
               Put_Line(my_newfile,"A=M-1");
               Put_Line(my_newfile,"D=M");
               Put_Line(my_newfile,"A=A-1");
               Put_Line(my_newfile,"M=M+D");
               Put_Line(my_newfile,"@SP");
               Put_Line(my_newfile,"M=M-1");
   end pr_add;

   procedure  pr_sub (my_newfile: File_Type) is
   begin
               Put_Line(my_newfile,("//sub"));
               Put_Line(my_newfile,"@SP");
               Put_Line(my_newfile,"A=M-1");
               Put_Line(my_newfile,"D=M");
               Put_Line(my_newfile,"A=A-1");
               Put_Line(my_newfile,"M=M-D");
               Put_Line(my_newfile,"@SP");
               Put_Line(my_newfile,"M=M-1");
   end pr_sub;

   procedure  pr_eq (my_newfile: File_Type) is
   begin
      Put_Line(my_newfile,("//eq"));
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"D=D-M");
      Put_Line(my_newfile,"@IF_TRUE" &Integer'Image(I));
      Put_Line(my_newfile,"D;JEQ");
      Put_Line(my_newfile,"D=0");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"M=D");
      Put_Line(my_newfile,"@IF_FALSE" &Integer'Image(I));
      Put_Line(my_newfile,"0;JMP");
      Put_Line(my_newfile,"(IF_TRUE"&Integer'Image(I) & ")");
      Put_Line(my_newfile,"D=-1");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"M=D");
      Put_Line(my_newfile,"(IF_FALSE"&Integer'Image(I) & ")");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M-1");
      I:=I+1;
      end pr_eq;

   procedure  pr_gt (my_newfile: File_Type) is

   begin
      Put_Line(my_newfile,("//gt"));
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"D=M-D");
      Put_Line(my_newfile,"@IF_TRUE" &Integer'Image(I));
      Put_Line(my_newfile,"D;JGT");
      Put_Line(my_newfile,"D=0");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"M=D");
      Put_Line(my_newfile,"@IF_FALSE" &Integer'Image(I));
      Put_Line(my_newfile,"0;JMP");
      Put_Line(my_newfile,"(IF_TRUE"&Integer'Image(I) & ")");
      Put_Line(my_newfile,"D=-1");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"M=D");
      Put_Line(my_newfile,"(IF_FALSE"&Integer'Image(I) & ")");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M-1");
      I:=I+1;
      end pr_gt;

   procedure  pr_lt (my_newfile: File_Type) is

   begin
      Put_Line(my_newfile,("//lt"));
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"D=D-M");
      Put_Line(my_newfile,"@IF_TRUE" &Integer'Image(I));
      Put_Line(my_newfile,"D;JGT");
      Put_Line(my_newfile,"D=0");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"M=D");
      Put_Line(my_newfile,"@IF_FALSE" &Integer'Image(I));
      Put_Line(my_newfile,"0;JMP");
      Put_Line(my_newfile,"(IF_TRUE"&Integer'Image(I) & ")");
      Put_Line(my_newfile,"D=-1");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"M=D");
      Put_Line(my_newfile,"(IF_FALSE"&Integer'Image(I) & ")");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M-1");
      I:=I+1;
      end pr_lt;

   procedure  pr_neg (my_newfile: File_Type) is

   begin
      Put_Line(my_newfile,("//neg"));
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"M=-D");
   end pr_neg;

   procedure  pr_not (my_newfile: File_Type) is

   begin
      Put_Line(my_newfile,("//not"));
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"A=M-1");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"M=!D");
   end pr_not;

   procedure  pr_and (my_newfile: File_Type) is
   begin
               Put_Line(my_newfile,("//and"));
               Put_Line(my_newfile,"@SP");
               Put_Line(my_newfile,"A=M-1");
               Put_Line(my_newfile,"D=M");
               Put_Line(my_newfile,"A=A-1");
               Put_Line(my_newfile,"M=D&M");
               Put_Line(my_newfile,"@SP");
               Put_Line(my_newfile,"M=M-1");
   end pr_and;

   procedure  pr_or(my_newfile: File_Type) is

   begin
               Put_Line(my_newfile,"//or");
               Put_Line(my_newfile,"@SP");
               Put_Line(my_newfile,"A=M-1");
               Put_Line(my_newfile,"D=M");
               Put_Line(my_newfile,"A=A-1");
               Put_Line(my_newfile,"M=M|D");
               Put_Line(my_newfile,"@SP");
               Put_Line(my_newfile,"M=M-1");
   end pr_or;

   procedure pr_lbl(my_newfile: File_Type; word2,m:String) is
   begin
      Put_Line(my_newfile,("//lable"));
      Put_Line(my_newfile,"("& word2 & "_" & m &")");



   end pr_lbl;

   procedure pr_ifgoto(my_newfile: File_Type; word2,m:String) is
   begin
      Put_Line(my_newfile,"//IF-GOTO -pop if !=0 jump");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M-1");
      Put_Line(my_newfile,"A=M");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@" & word2 & "_" & m );
      Put_Line(my_newfile,"D;JNE");


   end pr_ifgoto;

    procedure pr_goto(my_newfile: File_Type; word2,m:String) is
   begin
      Put_Line(my_newfile,"//GOTO");
      Put_Line(my_newfile,"@" & word2  & "_" & m);
      Put_Line(my_newfile,"0;JMP");


   end pr_goto;

    procedure pr_func(my_newfile: File_Type; word2,m,word3:String) is
   begin
      Put_Line(my_newfile,"//function");
      Put_Line(my_newfile,"("& word2 &")");
       for i in 1..Integer'Value(word3) loop
          pr_push_c(my_newfile,"0");
         end loop;




   end pr_func;

     procedure pr_return(my_newfile: File_Type) is
   begin
      Put_Line(my_newfile,"//return");
      Put_Line(my_newfile,"//put return address in R13-temp");

      Put_Line(my_newfile,"@LCL");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@5");
      Put_Line(my_newfile,"A=D-A");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@R13");
      Put_Line(my_newfile,"M=D");

      pr_pop(my_newfile,"ARG","0");


      Put_Line(my_newfile,"//sp => arg+1");
      Put_Line(my_newfile,"@ARG");
      Put_Line(my_newfile,"D=M+1");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=D");

      Put_Line(my_newfile,"//put back");
      Put_Line(my_newfile,"@LCL");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@1");
      Put_Line(my_newfile,"A=D-A");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@THAT");
      Put_Line(my_newfile,"M=D");

      Put_Line(my_newfile,"@LCL");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@2");
      Put_Line(my_newfile,"A=D-A");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@THIS");
      Put_Line(my_newfile,"M=D");


      Put_Line(my_newfile,"@LCL");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@3");
      Put_Line(my_newfile,"A=D-A");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@ARG");
      Put_Line(my_newfile,"M=D");


      Put_Line(my_newfile,"@LCL");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@4");
      Put_Line(my_newfile,"A=D-A");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@LCL");
      Put_Line(my_newfile,"M=D");


     Put_Line(my_newfile,"//jamp to R13");


      Put_Line(my_newfile,"@R13");
      Put_Line(my_newfile,"A=M");
      Put_Line(my_newfile,"0;JMP");



      end pr_return;

   procedure pr_call(my_newfile: File_Type; word2,word3:String) is
   begin
      c:=c+1;
      Put_Line(my_newfile,"//call");
       Put_Line(my_newfile,"//save RET LCL ARG THIS THAT");

      Put_Line(my_newfile,"@RETURN_ADDRESS" & Integer'Image(C));
      Put_Line(my_newfile,"D=A");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M+1");
      Put_Line(my_newfile,"A=M");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"M=D");

      Put_Line(my_newfile,"@LCL");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M+1");
      Put_Line(my_newfile,"A=M");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"M=D");

      Put_Line(my_newfile,"@ARG");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M+1");
      Put_Line(my_newfile,"A=M");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"M=D");

      Put_Line(my_newfile,"@THIS");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M+1");
      Put_Line(my_newfile,"A=M");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"M=D");

       Put_Line(my_newfile,"@THAT");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"M=M+1");
      Put_Line(my_newfile,"A=M");
      Put_Line(my_newfile,"A=A-1");
      Put_Line(my_newfile,"M=D");

      Put_Line(my_newfile,"//ARG=SP-numArg-5");

      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@5");
      Put_Line(my_newfile,"D=D-A");
      Put_Line(my_newfile,"@" & WORD3);
      Put_Line(my_newfile,"D=D-A");
      Put_Line(my_newfile,"@ARG");
      Put_Line(my_newfile,"M=D");

            Put_Line(my_newfile,"//LCL=SP");

      Put_Line(my_newfile,"@SP");
      Put_Line(my_newfile,"D=M");
      Put_Line(my_newfile,"@LCL");
      Put_Line(my_newfile,"M=D");
            Put_Line(my_newfile,"//GOTO F");

      Put_Line(my_newfile,"@" & word2);
      Put_Line(my_newfile,"0;JMP");
      Put_Line(my_newfile,"(" & "RETURN_ADDRESS" & Integer'Image(C) & ")");







   end pr_call;

begin
      Open(d_file,In_File,dd);
      while not End_Of_File(d_file)
   loop
      l:=To_Unbounded_String(get_Line(d_file));
      m:=To_Unbounded_String(get_Line(d_file));
   Open(my_file, In_File,To_String(l));
   Create(my_newfile, out_File, "C:\AAA EKRONOT\" & ".txt");

   while not End_Of_File(my_file)
      loop
         Put(get( my_file,Item => s));
         Put_Line(Get_Line(my_file));
     word:= oneword(my_file , b);

     if To_String(word)="push" then
        word2:=oneword(my_file,b);
            word3:=oneword(my_file,b);
         if word2="constant" then pr_push_c (my_newfile,To_String(word3));
         else if word2="this" then pr_push(my_newfile,"THIS",To_String(word3));
            else if word2="that" then pr_push(my_newfile,"THAT",To_String(word3));
               else if word2="local" then pr_push(my_newfile,"LCL",To_String(word3));
                  else if word2="argument" then pr_push(my_newfile,"ARG",To_String(word3));
                     else if word2="temp" then pr_push_t(my_newfile,To_String(word3));
                        else if word2="pointer" then pr_push_p(my_newfile,To_String(word3));
                              else if word2="static" then pr_push_st(my_newfile,To_String(word3));
                     end if; end if;  end if;  end if;  end if; end if; end if; end if;




      else if To_String(word)="pop" then
        word2:=oneword(my_file,b);
        word3:=oneword(my_file,b);
               if word2="constant" then pr_pop_c (my_newfile,To_String(word3));
         else if word2="this" then pr_pop(my_newfile,"THIS",To_String(word3));
               else if word2="that" then pr_pop(my_newfile,"THAT",To_String(word3));
                  else if word2="argument" then pr_pop(my_newfile,"ARG",To_String(word3));
                     else if word2="local" then pr_pop(my_newfile,"LCL",To_String(word3));
                        else if word2="temp" then pr_pop_t(my_newfile,To_String(word3));
                              else if word2="pointer" then pr_pop_p(my_newfile,To_String(word3));
                                   else if word2="static" then pr_pop_st(my_newfile,To_String(word3));

                       end if;  end if;  end if;  end if; end if;end if;end if; end if;


           else  if To_String(word)="sub" then
                  pr_sub(my_newfile);

            else if To_String(word)="add" then
                  pr_add(my_newfile);

            else if To_String(word)="eq" then
                  pr_eq(my_newfile);

            else if To_String(word)="lt" then
                  pr_lt(my_newfile);

            else if To_String(word)="gt" then
                  pr_gt(my_newfile);

           else if To_String(word)="not" then
                 pr_not(my_newfile);

            else if To_String(word)="and" then
                  pr_and(my_newfile);

            else if To_String(word)="or" then
                  pr_or(my_newfile);

            else if To_String(word)="neg" then
                  pr_neg(my_newfile);

            else if To_String(word)="label" then
                  word2:=oneword(my_file,b);
                  pr_lbl(my_newfile,To_String(word2),To_String(m));

            else if To_String(word)="if-goto" then
               word2:=oneword(my_file,b);
            pr_ifgoto(my_newfile,To_String(word2),To_String(m));

            else if To_String(word)="goto" then
               word2:=oneword(my_file,b);
            pr_goto(my_newfile,To_String(word2),To_String(m));

            else if To_String(word)="function" then
               word2:=oneword(my_file,b);
               word3:=oneword(my_file,b);

            pr_func(my_newfile,To_String(word2),To_String(m),To_String(word3));

            else if To_String(word)="return" then
               pr_return(my_newfile);

           else if To_String(word)="call" then
               word2:=oneword(my_file,b);
               word3:=oneword(my_file,b);
               pr_call(my_newfile,To_String(word2),To_String(word3));

            else l:=To_Unbounded_String(Get_Line(my_file));

               end if; end if;end if; end if; end if; end if;end if; end if; end if; end if; end if;
               end if; end if; end if; end if; end if; end if;

   end loop;

   Close(my_file);

      Close(my_newfile);
   end loop;
   Close(d_file);

   dir:=To_Unbounded_String("C:\AAA EKRONOT\Exercises\Targil2\project 08\FunctionCalls\FibonacciElement");
   Open(my_file, In_File,To_String(dir) & "\Main.asm");
   Create(my_newfile, Out_File,To_String(dir) & "\FibonacciElement.asm");
Put_Line(my_newfile,"// Initialize the SP to 256");
   Put_Line(my_newfile,"@256");
   Put_Line(my_newfile,"D=A");
   Put_Line(my_newfile,"@SP");
   Put_Line(my_newfile,"M=D");
   c:=0;
   pr_call(my_newfile,"Sys.init","0");


   while not End_Of_File(my_file) loop
      Put_Line(my_newfile,Get_Line(my_file));
   end loop;
   close(my_file);
      Open(my_file, In_File,To_String(dir) & "\Sys.asm");
   while not End_Of_File(my_file) loop
      Put_Line(my_newfile,Get_Line(my_file));
   end loop;
   close(my_file);
   Close(my_newfile);

   dir:=To_Unbounded_String("C:\AAA EKRONOT\Exercises\Targil2\project 08\FunctionCalls\StaticsTest");
   Create(my_newfile, Out_File,To_String(dir) & "\StaticsTest.asm");
Put_Line(my_newfile,"// Initialize the SP to 256");
   Put_Line(my_newfile,"@256");
   Put_Line(my_newfile,"D=A");
   Put_Line(my_newfile,"@SP");
   Put_Line(my_newfile,"M=D");
   c:=0;
   pr_call(my_newfile,"Sys.init","0");

    Open(my_file, In_File,To_String(dir) & "\Class1.asm");

   while not End_Of_File(my_file) loop
      Put_Line(my_newfile,Get_Line(my_file));
   end loop;
   close(my_file);
      Open(my_file, In_File,To_String(dir) & "\Class2.asm");
   while not End_Of_File(my_file) loop
      Put_Line(my_newfile,Get_Line(my_file));
   end loop;
   close(my_file);
   Open(my_file, In_File,To_String(dir) & "\Sys.asm");
   while not End_Of_File(my_file) loop
      Put_Line(my_newfile,Get_Line(my_file));
   end loop;
   Close(my_file);
   Close(my_newfile);




end Main;
