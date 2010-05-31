with ARM_Output,
     ARM_Contents,
     Ada.Text_IO;
-- private
with Ada.Strings.Unbounded;
package ARM_RTF is

    --
    -- Ada reference manual formatter.
    --
    -- This package defines the RTF output object.
    -- Output objects are responsible for implementing the details of
    -- a particular format.
    --
    -- ---------------------------------------
    -- Copyright 2000, 2002, 2004, 2005, 2006  AXE Consultants.
    -- P.O. Box 1512, Madison WI  53701
    -- E-Mail: randy@rrsoftware.com
    --
    -- AXE Consultants grants to all users the right to use/modify this
    -- formatting tool for non-commercial purposes. (ISO/IEC JTC 1 SC 22 WG 9
    -- activities are explicitly included as "non-commercial purposes".)
    -- Commercial uses of this software and its source code, including but not
    -- limited to documents for sale and sales of modified versions of this
    -- tool, are prohibited without the prior written permission of
    -- AXE Consultants. All rights not explicitly granted above are reserved
    -- by AXE Consultants.
    --
    -- You use this tool and/or its source code on the condition that you indemnify and hold harmless
    -- AXE Consultants, its agents, and employees, from any and all liability
    -- or damages to yourself or your hardware or software, or third parties,
    -- including attorneys' fees, court costs, and other related costs and
    -- expenses, arising out of your use of this tool and/or source code irrespective of the
    -- cause of said liability.
    --
    -- AXE CONSULTANTS MAKES THIS TOOL AND SOURCE CODE AVAILABLE ON AN "AS IS"
    -- BASIS AND MAKES NO WARRANTY, EXPRESS OR IMPLIED, AS TO THE ACCURACY,
    -- CAPABILITY, EFFICIENCY, MERCHANTABILITY, OR FUNCTIONING OF THIS TOOL.
    -- IN NO EVENT WILL AXE CONSULTANTS BE LIABLE FOR ANY GENERAL,
    -- CONSEQUENTIAL, INDIRECT, INCIDENTAL, EXEMPLARY, OR SPECIAL DAMAGES,
    -- EVEN IF AXE CONSULTANTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
    -- DAMAGES.
    -- ---------------------------------------
    --
    -- Edit History:
    --
    --  5/18/00 - RLB - Created package.
    --  5/22/00 - RLB - Added Includes_Changes to Create.
    --  5/23/00 - RLB - Added Set_Column and New_Column.
    --		      - Added Tab_Info and Tab_Stops.
    --  5/24/00 - RLB - Added Location to Text_Format.
    --		- RLB - Added No_Breaks and Keep_with_Next to Start_Paragraph.
    --  5/25/00 - RLB - Added Big_Files to Create. Added Justification.
    --		- RLB - Added Separator_Lines and TOC routines.
    --  5/26/00 - RLB - Added table operations.
    --  6/ 2/00 - RLB - Added Soft_Line_Break.
    --  8/ 2/00 - RLB - Added Soft_Hyphen_Break.
    --  8/ 7/00 - RLB - Added Leading flag to Start_Paragraph.
    --  8/17/00 - RLB - Replaced "Leading" by "Space_After".
    --  8/22/00 - RLB - Added Revised_Clause_Header.
    --  7/18/02 - RLB - Removed Document parameter from Create, replaced by
    --			three strings and For_ISO boolean.
    --		- RLB - Added AI_Reference.
    --		- RLB - Added Change_Version_Type and uses.
    --  9/10/04 - RLB - Added "Both" to possible changes to handle
    --			replacement of changed text.
    --  9/14/04 - RLB - Moved Change_Version_Type to ARM_Contents.
    --  5/27/05 - RLB - Added arbitrary Unicode characters.
    --  1/11/06 - RLB - Eliminated dispatching Create in favor of tailored
    --			versions.
    --  1/13/06 - RLB - Added new Link operations.
    --  2/ 8/06 - RLB - Added additional parameters to the table command.
    --  2/10/06 - RLB - Added even more additional parameters to the
    --			table command.
    --		- RLB - Added picture command.
    --  3/30/06 - RLB - Added shape id counter.
    --  9/21/06 - RLB - Added Body_Font.
    --  9/25/06 - RLB - Added Last_Column_Width to Start_Table.
    -- 10/13/06 - RLB - Added Local_Link_Start and Local_Link_End to allow
    --			formatting in the linked text.

    type RTF_Output_Type is new ARM_Output.Output_Type with private;

    type Page_Size is (A4, Letter, Ada95, Half_Letter);
	-- A4 is standard European letter size.
	-- Letter is standard American letter size (8.5x11).
	-- Half_Letter is standard America half size (5.5x8.5).
	-- Ada95 is the size of the existing Ada 95 standard (7x9).

    type Serif_Fonts is (Times_New_Roman, Souvenir);
    type Sans_Serif_Fonts is (Arial, Helvetica);

    procedure Create (Output_Object : in out RTF_Output_Type;
		      Page_Size : in ARM_RTF.Page_Size;
		      Includes_Changes : in Boolean;
		      Big_Files : in Boolean;
		      Primary_Sans_Serif_Font : in Sans_Serif_Fonts := Arial;
		      Primary_Serif_Font : in Serif_Fonts := Times_New_Roman;
		      Body_Font : in ARM_Output.Font_Family_Type := ARM_Output.Roman;
		      File_Prefix : in String;
		      Header_Prefix : in String := "";
		      Title : in String := "");
	-- Create an Output_Object for a document with the specified page
	-- size. Changes from the base document are included if
	-- Includes_Changes is True (otherwise no revisions are generated).
	-- Generate a few large output files if
	-- Big_Files is True; otherwise generate smaller output files.
	-- The prefix of the output file names is File_Prefix - this
	-- should be no more then 4 characters allowed in file names.
	-- The title of the document is Title.
	-- The header prefix appears in the header (if any) before the title,
	-- separated by a dash.
	-- The primary font used for the Sans_Serif text, and for the Serif
	-- text, is as specified.
	-- Which font is used for the body is specified by Body_Font.

    procedure Close (Output_Object : in out RTF_Output_Type);
	-- Close an Output_Object. No further output to the object is
	-- allowed after this call.


    procedure Section (Output_Object : in out RTF_Output_Type;
		       Section_Title : in String;
		       Section_Name : in String);
	-- Start a new section. The title is Section_Title (this is
	-- intended for humans). The name is Section_Name (this is
	-- intended to be suitable to be a portion of a file name).

    procedure Set_Columns (Output_Object : in out RTF_Output_Type;
			   Number_of_Columns : in ARM_Output.Column_Count);
	-- Set the number of columns.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Start_Paragraph (Output_Object : in out RTF_Output_Type;
			       Format : in ARM_Output.Paragraph_Type;
			       Number : in String;
			       No_Prefix : in Boolean := False;
			       Tab_Stops : in ARM_Output.Tab_Info := ARM_Output.NO_TABS;
			       No_Breaks : in Boolean := False;
			       Keep_with_Next : in Boolean := False;
			       Space_After : in ARM_Output.Space_After_Type
				   := ARM_Output.Normal;
			       Justification : in ARM_Output.Justification_Type
				   := ARM_Output.Default);
	-- Start a new paragraph. The format of the paragraph is as specified.
	-- The (AA)RM paragraph number (which might include update and version
	-- numbers as well: [12.1/1]) is Number. If the format is a type with
	-- a prefix (bullets, hangining items), the prefix is omitted if
	-- No_Prefix is true. Tab_Stops defines the tab stops for the
	-- paragraph. If No_Breaks is True, we will try to avoid page breaks
	-- in the paragraph. If Keep_with_Next is true, we will try to avoid
	-- separating this paragraph and the next one. (These may have no
	-- effect in formats that don't have page breaks). Space_After
	-- specifies the amount of space following the paragraph. Justification
	-- specifies the text justification for the paragraph. Not_Valid_Error
	-- is raised if Tab_Stops /= NO_TABS for a hanging or bulleted format.

    procedure End_Paragraph (Output_Object : in out RTF_Output_Type);
	-- End a paragraph.

    procedure Category_Header (Output_Object : in out RTF_Output_Type;
			       Header_Text : String);
	-- Output a Category header (that is, "Legality Rules",
	-- "Dynamic Semantics", etc.)
	-- (Note: We did not use a enumeration here to insure that these
	-- headers are spelled the same in all output versions).
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Clause_Header (Output_Object : in out RTF_Output_Type;
			     Header_Text : in String;
			     Level : in ARM_Contents.Level_Type;
			     Clause_Number : in String;
			     No_Page_Break : in Boolean := False);
	-- Output a Clause header. The level of the header is specified
	-- in Level. The Clause Number is as specified.
	-- These should appear in the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Revised_Clause_Header (Output_Object : in out RTF_Output_Type;
			     New_Header_Text : in String;
			     Old_Header_Text : in String;
			     Level : in ARM_Contents.Level_Type;
			     Clause_Number : in String;
			     Version : in ARM_Contents.Change_Version_Type;
			     No_Page_Break : in Boolean := False);
	-- Output a revised clause header. Both the original and new text will
	-- be output. The level of the header is specified in Level. The Clause
	-- Number is as specified.
	-- These should appear in the table of contents.
	-- For hyperlinked formats, this should generate a link target.
	-- If No_Page_Break is True, suppress any page breaks.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure TOC_Marker (Output_Object : in out RTF_Output_Type;
			  For_Start : in Boolean);
	-- Mark the start (if For_Start is True) or end (if For_Start is
	-- False) of the table of contents data. Output objects that
	-- auto-generate the table of contents can use this to do needed
	-- actions.

    procedure New_Page (Output_Object : in out RTF_Output_Type;
			Kind : ARM_Output.Page_Kind_Type := ARM_Output.Any_Page);
	-- Output a page break.
	-- Note that this has no effect on non-printing formats.
	-- Any_Page breaks to the top of the next page (whatever it is);
	-- Odd_Page_Only breaks to the top of the odd-numbered page;
	-- Soft_Page allows a page break but does not force one (use in
	-- "No_Breaks" paragraphs.)
	-- Raises Not_Valid_Error if in a paragraph if Kind = Any_Page or
	-- Odd_Page, and if not in a paragraph if Kind = Soft_Page.

    procedure New_Column (Output_Object : in out RTF_Output_Type);
	-- Output a column break.
	-- Raises Not_Valid_Error if in a paragraph, or if the number of
	-- columns is 1.

    procedure Separator_Line (Output_Object : in out RTF_Output_Type;
			      Is_Thin : Boolean := True);
	-- Output a separator line. It is thin if "Is_Thin" is true.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Start_Table (Output_Object : in out RTF_Output_Type;
			   Columns : in ARM_Output.Column_Count;
			   First_Column_Width : in ARM_Output.Column_Count;
			   Last_Column_Width : in ARM_Output.Column_Count;
			   Alignment : in ARM_Output.Column_Text_Alignment;
			   No_Page_Break : in Boolean;
			   Has_Border : in Boolean;
			   Small_Text_Size : in Boolean;
			   Header_Kind : in ARM_Output.Header_Kind_Type);
	-- Starts a table. The number of columns is Columns; the first
	-- column has First_Column_Width times the normal column width, and
	-- the last column has Last_Column_Width times the normal column width.
	-- Alignment is the horizontal text alignment within the columns.
	-- No_Page_Break should be True to keep the table intact on a single
	-- page; False to allow it to be split across pages.
	-- Has_Border should be true if a border is desired, false otherwise.
	-- Small_Text_Size means that the contents will have the AARM size;
	-- otherwise it will have the normal size.
	-- Header_Kind determines whether the table has headers.
	-- This command starts a paragraph; the entire table is a single
	-- paragraph. Text will be considered part of the caption until the
	-- next table marker call.
	-- Raises Not_Valid_Error if in a paragraph.

    procedure Table_Marker (Output_Object : in out RTF_Output_Type;
			    Marker : in ARM_Output.Table_Marker_Type);
	-- Marks the end of an entity in a table.
	-- If Marker is End_Caption, the table caption ends and the
	--	future text is part of the table header.
	-- If Marker is End_Header, the table header ends and the
	--	future text is part of the table body.
	-- If Marker is End_Row, a row in the table is completed, and another
	--	row started.
	-- If Marker is End_Item, an item in the table header or body is ended,
	--	and another started.
	-- If Marker is End_Table, the entire table is finished.
	-- Raises Not_Valid_Error if not in a table.

    -- Text output: These are only allowed after a Start_Paragraph and
    -- before any End_Paragraph. Raises Not_Valid_Error if not in a paragraph,
    -- or another error.

    procedure Ordinary_Text (Output_Object : in out RTF_Output_Type;
			     Text : in String);
	-- Output ordinary text.
	-- The text must end at a word break, never in the middle of a word.

    procedure Ordinary_Character (Output_Object : in out RTF_Output_Type;
			          Char : in Character);
	-- Output an ordinary character.
	-- Spaces will be used to break lines as needed.

    procedure Hard_Space (Output_Object : in out RTF_Output_Type);
	-- Output a hard space. No line break should happen at a hard space.

    procedure Line_Break (Output_Object : in out RTF_Output_Type);
	-- Output a line break. This does not start a new paragraph.
	-- This corresponds to a "<BR>" in RTF.

    procedure Index_Line_Break (Output_Object : in out RTF_Output_Type;
				Clear_Keep_with_Next : in Boolean);
	-- Output a line break for the index. This does not start a new
	-- paragraph in terms of spacing. This corresponds to a "<BR>"
	-- in HTML. If Clear_Keep_with_Next is true, insure that the next
	-- line does not require the following line to stay with it.
	-- Raises Not_Valid_Error if the paragraph is not in the index format.

    procedure Soft_Line_Break (Output_Object : in out RTF_Output_Type);
	-- Output a soft line break. This is a place (in the middle of a
	-- "word") that we allow a line break. It is usually used after
	-- underscores in long non-terminals.

    procedure Soft_Hyphen_Break (Output_Object : in out RTF_Output_Type);
	-- Output a soft line break, with a hyphen. This is a place (in the middle of
	-- a "word") that we allow a line break. If the line break is used,
	-- a hyphen will be added to the text.

    procedure Tab (Output_Object : in out RTF_Output_Type);
	-- Output a tab, inserting space up to the next tab stop.
	-- Raises Not_Valid_Error if the paragraph was created with
	-- Tab_Stops = ARM_Output.NO_TABS.

    procedure Special_Character (Output_Object : in out RTF_Output_Type;
			         Char : in ARM_Output.Special_Character_Type);
	-- Output an special character.

    procedure Unicode_Character (Output_Object : in out RTF_Output_Type;
			         Char : in ARM_Output.Unicode_Type);
	-- Output a Unicode character, with code position Char.

    procedure End_Hang_Item (Output_Object : in out RTF_Output_Type);
	-- Marks the end of a hanging item. Call only once per paragraph.
	-- Raises Not_Valid_Error if the paragraph format is not
	-- Hanging .. Small_Nested_Enumerated, or if this has already been
	-- called for the current paragraph, or if the paragraph was started
	-- with No_Prefix = True.

    procedure Text_Format (Output_Object : in out RTF_Output_Type;
			   Bold : in Boolean;
			   Italic : in Boolean;
			   Font : in ARM_Output.Font_Family_Type;
			   Size : in ARM_Output.Size_Type;
			   Change : in ARM_Output.Change_Type;
			   Version : in ARM_Contents.Change_Version_Type := '0';
			   Added_Version : in ARM_Contents.Change_Version_Type := '0';
			   Location : in ARM_Output.Location_Type);
	-- Change the text format so that Bold, Italics, the font family,
	-- the text size, and the change state are as specified.
	-- Added_Version is only used when the change state is "Both"; it's
	-- the version of the insertion; Version is the version of the (newer)
	-- deletion.
	-- Note: Changes to these properties ought be stack-like; that is,
	-- Bold on, Italic on, Italic off, Bold off is OK; Bold on, Italic on,
	-- Bold off, Italic off should be avoided (as separate commands).

    procedure Clause_Reference (Output_Object : in out RTF_Output_Type;
				Text : in String;
				Clause_Number : in String);
	-- Generate a reference to a clause in the standard. The text of
	-- the reference is "text", and the number of the clause is
	-- Clause_Number. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure Index_Target (Output_Object : in out RTF_Output_Type;
			    Index_Key : in Natural);
	-- Generate a index target. This marks the location where an index
	-- reference occurs. Index_Key names the index item involved.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, nothing is generated.

    procedure Index_Reference (Output_Object : in out RTF_Output_Type;
			       Text : in String;
			       Index_Key : in Natural;
			       Clause_Number : in String);
	-- Generate a reference to an index target in the standard. The text
	-- of the reference is "Text", and Index_Key and Clause_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure DR_Reference (Output_Object : in out RTF_Output_Type;
			    Text : in String;
			    DR_Number : in String);
	-- Generate a reference to an DR from the standard. The text
	-- of the reference is "Text", and DR_Number denotes
	-- the target. For hyperlinked formats, this should generate
	-- a link; for other formats, the text alone is generated.

    procedure AI_Reference (Output_Object : in out RTF_Output_Type;
			    Text : in String;
			    AI_Number : in String);
	-- Generate a reference to an AI from the standard. The text
	-- of the reference is "Text", and AI_Number denotes
	-- the target (in folded format). For hyperlinked formats, this should
	-- generate a link; for other formats, the text alone is generated.

    procedure Local_Target (Output_Object : in out RTF_Output_Type;
			    Text : in String;
			    Target : in String);
	-- Generate a local target. This marks the potential target of local
	-- links identified by "Target". Text is the text of the target.
	-- For hyperlinked formats, this should generate a link target;
	-- for other formats, only the text is generated.

    procedure Local_Link (Output_Object : in out RTF_Output_Type;
			  Text : in String;
			  Target : in String;
			  Clause_Number : in String);
	-- Generate a local link to the target and clause given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Local_Link_Start (Output_Object : in out RTF_Output_Type;
				Target : in String;
				Clause_Number : in String);
	-- Generate a local link to the target and clause given.
	-- The link will surround text until Local_Link_End is called.
	-- Local_Link_End must be called before this routine can be used again.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Local_Link_End (Output_Object : in out RTF_Output_Type;
			      Target : in String;
			      Clause_Number : in String);
	-- End a local link for the target and clause given.
	-- This must be in the same paragraph as the Local_Link_Start.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure URL_Link (Output_Object : in out RTF_Output_Type;
			Text : in String;
			URL : in String);
	-- Generate a link to the URL given.
	-- Text is the text of the link.
	-- For hyperlinked formats, this should generate a link;
	-- for other formats, only the text is generated.

    procedure Picture  (Output_Object : in out RTF_Output_Type;
			Name  : in String;
			Descr : in String;
			Alignment : in ARM_Output.Picture_Alignment;
			Height, Width : in Natural;
			Border : in ARM_Output.Border_Kind);
	-- Generate a picture.
	-- Name is the (simple) file name of the picture; Descr is a
	-- descriptive name for the picture (it will appear in some web
	-- browsers).
	-- We assume that it is a .PNG or .JPG and that it will be present
	-- in the same directory as the output files.
	-- Alignment specifies the picture alignment.
	-- Height and Width specify the picture size in pixels.
	-- Border specifies the kind of border.

private

    subtype Prefix_String is String(1..4);
    type RTF_Output_Type is new ARM_Output.Output_Type with record
	Is_Valid : Boolean := False;
	Is_In_Paragraph : Boolean := False;
	Paragraph_Format : ARM_Output.Paragraph_Type := ARM_Output.Normal;
	Had_Prefix : Boolean := False; -- If in paragraph, value of (not No_Prefix).
	Wrote_into_Section : Boolean := False; -- Have we written into the
		-- current section yet?
	Column_Count : ARM_Output.Column_Count := 1; -- Number of columns in current section.
	Output_File : Ada.Text_IO.File_Type;
	File_Prefix : Prefix_String; -- Blank padded.
	Title : Ada.Strings.Unbounded.Unbounded_String;
	Header_Prefix : Ada.Strings.Unbounded.Unbounded_String;
	Page_Size : ARM_RTF.Page_Size;
	Includes_Changes : Boolean;
	Big_Files : Boolean; -- For RTF, this means to generate a single monster file.
	Primary_Sans_Serif_Font : Sans_Serif_Fonts;
	Primary_Serif_Font : Serif_Fonts;
	Body_Font : ARM_Output.Font_Family_Type;
	For_ISO : Boolean;
	Char_Count : Natural := 0; -- Characters on current line.
	Saw_Hang_End : Boolean := False; -- If we are in a hanging paragraph,
			       -- have we seen the end of the hanging part yet?
	Current_Space_After : ARM_Output.Space_After_Type := ARM_Output.Normal;
				-- The value of Space_After for the current
				-- paragraph.
	Prefix_Large_Char_Count : Natural := 0;
		-- If we're in a hanging paragraph, and Saw_Hang_End is False,
		-- this is a count of the large (capitals, mostly) characters
		-- visible in the prefix.
	Is_Bold : Boolean; -- Is the text currently bold?
	Is_Italic : Boolean; -- Is the text current italics?
	Font : ARM_Output.Font_Family_Type; -- What is the current font family?
	Size : ARM_Output.Size_Type; -- What is the current relative size?
	Real_Size : Natural; -- What is the current size in halfpoints?
	Change : ARM_Output.Change_Type := ARM_Output.None;
	Version : ARM_Contents.Change_Version_Type := '0';
	Added_Version : ARM_Contents.Change_Version_Type := '0';
	Location : ARM_Output.Location_Type := ARM_Output.Normal;
	Tab_Stops : ARM_Output.Tab_Info := ARM_Output.NO_TABS;
	-- Tables:
	Is_In_Table : Boolean := False; -- Are we processing a table?
	Table_Width : Natural := 0; -- The width of the table, in twips.
	Table_Indent : Natural := 0; -- The indent of the table, in twips.
	Table_Column_Width : Natural := 0; -- The column width of the table, in twips.
	Table_First_Column_Mult : ARM_Output.Column_Count := 1; -- The multiple of the first column.
	Table_Last_Column_Mult : ARM_Output.Column_Count := 1; -- The multiple of the last column.
	Table_Alignment : ARM_Output.Column_Text_Alignment := ARM_Output.Center_All;
	Table_No_Page_Break : Boolean := False; -- Is a page break allowed in the table?
	Table_Has_Border : Boolean := False; -- Does the table have a border?
	Table_Has_Small_Text : Boolean := False; -- Does the table have small text?
	-- Pictures:
	Last_Shape_Id : Natural := 1024; -- Shape ids.
    end record;

end ARM_RTF;
