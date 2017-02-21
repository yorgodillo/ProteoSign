var sessionid = new Date().getTime();
var clientname = '';
var softversion = '';
var cgi_bin_path = 'cgi-bin/';
var uploaded_files = 0;
var upload_was_successful = false;
var types_of_files_uploaded = [];
var nToUpload = 0;
var tmp_nToUpload = 0;
var explblselected = [];
var AddedLabels = false;
var AppendNewLabels = false;
var isLabelFree = false;
var LFQconditions = [];
var RawFileConditions = [];
var isIsobaricLabel = false;
var analysis_finished = false;
var datatestOK_clicked = false;
//procProgram refers to the program that is believed to have processed the raw data (MQ or PD) based on the uploaded files' format
var procProgram = "";
var my_lbls_toselect = [];
var RenameArray = []; //used to cover the possibility where more than one labels correspond to the same condition
var AuthenticItemsinRename = []; //used to cover the possibility where more than one labels correspond to the same condition
var RenameFromTestData = false;
var itemsToRename = [];
var my_all_mq_labels = "";
//DEBUG flags
var AllowMergeLabels = true;
var AllowLS = true;
//LabelSwap vars:
var LS_array_counter = 0;
var LS_counters_per_Add = [];//each of LS_counters_per_Add's elements is an array that contains as a first element the decription of the Label swap as shown in LSLabelSwaps list and second another array with the indices of the swaps in LabelSwapArray that correspond to the specific list element a third array contains two values which are the labels that correspond to this ls swap assignment
var LabelSwapArray = [];//Format: first column ([0]): a rawfile where the swap occurs, second ([1]): the first label that is swapped, third ([2]): the second label of the swap and fourth ([3]): a unique index
var LSselected_raws = [];
// from: http://www.shamasis.net/2009/09/fast-algorithm-to-find-unique-items-in-javascript-array/
Array.prototype.unique = function () {
   var o = {}, i, l = this.length, r = [];
   for (i = 0; i < l; i += 1)
      o[this[i]] = this[i];
   for (i in o)
      r.push(o[i]);
   return r;

}

//For unique arrays:
//from http://stackoverflow.com/questions/6940103/how-do-i-make-an-array-with-unique-elements-i-e-remove-duplicates
function ArrNoDupe(a) {
    var temp = {};
    for (var i = 0; i < a.length; i++)
        temp[a[i]] = true;
    var r = [];
    for (var k in temp)
        r.push(k);
    return r;
}

// for counting the number of elements in an associative array
// from: http://stackoverflow.com/questions/5223/length-of-javascript-object-ie-associative-array
Object.size = function (obj) {
   var size = 0, key;
   for (key in obj) {
      if (obj.hasOwnProperty(key))
         size++;
   }
   return size;
}
// for filtering filetypes
function filterFunction(value) {
	if (procProgram == "MQ")
	{
		return (value == "MQ" || value == "MQP");
	}
	else if( procProgram == "PD")
	{
		return (value == "PD");
	}
}
//http://dzone.com/snippets/array-shuffle-javascript
function shuffle(o) {
   for (var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x)
      ;
   return o;
}
;
File.prototype.toString = function getFileName() {
   return this.name;
}

var rawfiles_tbl_allfiles_DT;
var main_context_menu;

var onChooseFromTestDatasets = function(){
	if (!datatestOK_clicked)
	{
		$(".expparamsDlg").css({"left" : ($("body").width()/2) - ($("#s1TestDatasets").width()/2)});
		$('body').append('<div id="mask"></div>');
		$("#s1TestDatasets").fadeIn(300);
		$('#mask').fadeIn(300);
	}
}

var onAssignCondition = function(){
	$(".expparamsDlg").css({"left" : ($("body").width()/2) - ($("#s2LFQConditions").width()/2)});
	$('body').append('<div id="mask"></div>');
	$("#s2LFQConditions").fadeIn(300);
	$('#mask').fadeIn(300);
	$("#s2LFQConditionsOK").on("click", function () {
         dlgFadeout();
		 //ADD OK RESULT HERE
    });
	$("#s2LFQConditionsCancel").on("click", function () {
         dlgFadeout();
		 //ADD cancel RESULT HERE
    });
}

var create_my_all_mq_labels = function()
{
	if (!RenameFromTestData)
	{
		var myOpts = document.getElementById('conditions_list').options;
		my_all_mq_labels = "c(";
		for(i = 0; i < myOpts.length; i++)
		{
			if(i != myOpts.length - 1)
			{
				my_all_mq_labels = my_all_mq_labels + '"' + myOpts[i].value + '", ';
			}
			else
			{
				my_all_mq_labels = my_all_mq_labels + '"' + myOpts[i].value + '"';
			}
		}
		
		my_all_mq_labels = my_all_mq_labels + ")";
	}
	else
	{
		my_all_mq_labels = "c(";
		for(i = 0; i < RenameArray.length; i++)
		{
			if(i != RenameArray.length - 1)
			{
				my_all_mq_labels = my_all_mq_labels + '"' + RenameArray[i][0] + '", ';
			}
			else
			{
				my_all_mq_labels = my_all_mq_labels + '"' + RenameArray[i][0] + '"';
			}
		}
		
		my_all_mq_labels = my_all_mq_labels + ")";
	}
}

var InitializeRename = function()
{
	RenameArray = [];
	AuthenticItemsinRename = [];
	var conditions_in_list = [];
	$("#conditions_list option").each(function(){ conditions_in_list.push($(this).val());})
	$.each(conditions_in_list, function(idx, my_cond)
	{
		var value_to_push = [];
		value_to_push[0] = my_cond;
		value_to_push[1] = my_cond;
		RenameArray.push(value_to_push);
		AuthenticItemsinRename.push(value_to_push[0]);
	});
	// console.log(AuthenticItemsinRename);
	Refresh_conds_list_cmenu_items();
}

var Refresh_conds_list = function()
{
	if (!AllowMergeLabels) return;
	$("#conditions_list").empty();
	var temp_array = [];
	$.each(RenameArray, function(idx, my_row)
	{
		temp_array.push(my_row[1]);
	});
	var unique_conditions = ArrNoDupe(temp_array);
	$.each(unique_conditions, function (idx, my_un_cond)
	{
		if($.inArray(my_un_cond, AuthenticItemsinRename) == -1)
		{
			$("#conditions_list").append("<option value='" + my_un_cond + "' style='padding: 2px; font-size: 125%; font-weight: bold;' selected='true'>" + my_un_cond + "</option>");
		}
		else
		{
			$("#conditions_list").append("<option value='" + my_un_cond + "' style='padding: 2px; font-size: 125%;' selected='true'>" + my_un_cond + "</option>");
		}
		
	});
	//Make sure that expquantfiltlbl containing the label used for quantitation filtering contains a valid label after merging, in case the user had selected one label that became invalid prompt him/her by making expquantfiltlbl's border red
	var my_sel_item = "";
	if (!RenameFromTestData)
	{
		my_sel_item = $("#expquantfiltlbl option:selected").val();
	}
	$("#expquantfiltlbl").empty();
	$.each(unique_conditions, function (idx, my_un_cond)
	{
		$("#expquantfiltlbl").append("<option oncontextmenu='return false;' value='" + my_un_cond + "'>" + my_un_cond + "</option>");
	});
	if (!RenameFromTestData && my_sel_item !== "")
	{
		var prev_selected_item_exists = false;
		$("#expquantfiltlbl option").each(function(idx, my_option)
		{
			if($(this).val() == my_sel_item)
			{
				prev_selected_item_exists = true;
			}
		});
		if(prev_selected_item_exists)
		{
			$("#expquantfiltlbl").val(my_sel_item);
		}
		else
		{
			if($("#expquantfilt").prop("checked") == true)
			{
				setItemAttrColor("#expquantfiltlbl", "border", "#E60000");
			}
		}
	}
	//Make sure that after altering the valid labels, there are no label swaps assigned that contain invalid labels. In such a case delete the swaps and prompt the user
	//First get all current valid labels:
	if (!AllowLS) return;
	var my_valid_options = [];
	$("#conditions_list option").each(function(idx, my_valid_opt)
	{
		my_valid_options.push($(my_valid_opt).val());
	});
	var my_invalid_LSwaps = [];
	if (!RenameFromTestData)
	{
		//the last element of LS_counters_per_Add is an array of the labels that are swapped in a single swap assignment
		$.each(LS_counters_per_Add, function(idx, my_swap_assignment)
		{
			//in case at least one of the labels in an assignment is not valid remove it from the LSwaps and inform the user
			if($.inArray(my_swap_assignment[2][0], my_valid_options) == -1 || $.inArray(my_swap_assignment[2][1], my_valid_options) == -1)
			{
				my_invalid_LSwaps.push(my_swap_assignment[0]);
			}
		});
	}
	//Now my_invalid_LSwaps contains the descriptions of all the LSwaps assignments to be removed, select them in LSLabelSwaps and call onLSRemoveclick to remove them safely
	if(my_invalid_LSwaps.length >0 )
	{
		//Note that if a LS assignment has been removed manually or not in the past, it is candidate to be considered invalid again since no function erases any elements of LS_counters_per_Add. So before trying to remove an LSassignment always check if it exists in LSLabelSwaps list
		var real_invalid_LSs = 0;
		var text_to_display = "";
		$("#LSLabelSwaps option").each(function(idx, my_opt)
		{
			if ($.inArray($(my_opt).val(), my_invalid_LSwaps) != -1)
			{
				$(my_opt).prop("selected", true);
				text_to_display = text_to_display + '<i>"' + $(my_opt).val() + '"</i>, ';
				real_invalid_LSs++;
			}
			else
			{
				$(my_opt).prop("selected", false);
			}
		});
		if (real_invalid_LSs > 1)
		{
			onLSRemoveclick();
			text_to_display = "Label swaps: " + text_to_display;
			text_to_display = text_to_display + "are no longer valid and were removed";
			msgbox(text_to_display);//Prompt the user
		}
		else if(real_invalid_LSs == 1)
		{
			onLSRemoveclick();
			text_to_display = "Label swap: " + text_to_display;
			text_to_display = text_to_display + "is no longer valid and was removed";
			msgbox(text_to_display);//Prompt the user
		}
	}
}

var Refresh_conds_list_cmenu_items = function()
{
	//Check which items on the cmenu of conditions list should be visible Merge button:
	var my_items = [];
	var show_Merge = true;//Show merge if and only if all selected options are authentic
	$("#conditions_list option:selected").each(function(idx, my_item){
		if ($.inArray($(this).val(), AuthenticItemsinRename) == -1)
		{
			show_Merge = false;
		}
		my_items.push(my_item);
	});
	// console.log(my_items.length);
	if(my_items.length < 2)
	{
		show_Merge = false;
	}
	$("#conds_list_container").contextmenu("showEntry", "same_cond", show_Merge);
	//Check which items on the cmenu of conditions list should be visible Restore button:
	var my_items = [];
	var show_restore = true;//Show merge if and only if all selected options are authentic
	$("#conditions_list option:selected").each(function(idx, my_item){
		if ($.inArray($(this).val(), AuthenticItemsinRename) != -1)
		{
			show_restore = false;
		}
		my_items.push(my_item);
	});
	// console.log(my_items.length);
	if(my_items.length == 0)
	{
		show_restore = false;
	}
	$("#conds_list_container").contextmenu("showEntry", "restore_conds", show_restore);
	if (!show_restore && !show_Merge)
	{
		$('#conds_list_container').contextmenu("option", "autoTrigger", false);
	}
	else
	{
		$('#conds_list_container').contextmenu("option", "autoTrigger", true);
	}
}

var InitializeLS = function()
{
	if (!AllowLS) return;
	//initialize the popup div
	//first get all the bioreps available
	var my_bioreps = [];
	var my_techreps = [];
	 $.each(rawfiles_structure, function (idx, my_raw_file)
	 {
		 if (my_raw_file.used == false) return;
		 my_bioreps.push(my_raw_file.biorep);
		 my_techreps.push(my_raw_file.techrep);
	 });
	 var unique_breps = ArrNoDupe(my_bioreps);
	 var unique_treps = ArrNoDupe(my_techreps);
	 $("#LSbrep").empty();
	 $("#LStrep").empty();
	 $("#LSbrep").append("<option value='" + "BioReps" + "' selected='true'>" + "Bio Reps" + "</option>");
	 $("#LStrep").append("<option value='" + "TechReps" + "' selected='true'>" + "Tech Reps" + "</option>");
	 $.each(unique_breps, function (idx, my_brep)
	 {
		 $("#LSbrep").append("<option value='" + my_brep + "'>" + my_brep + "</option>");
	 });
	 $.each(unique_treps, function (idx, my_trep)
	 {
		 $("#LStrep").append("<option value='" + my_trep + "'>" + my_trep + "</option>");
	 });
	 //now refresh the label selects
	 $("#LSfirstlabel").empty();
	 $("#LSsecondlabel").empty();
	 $("#LSRawFilesList").empty();
	$("#conditions_list option").each(function(idx, my_item){
		
		$("#LSfirstlabel").append("<option value='" + $(this).val() + "'>" + $(this).val() + "</option>");
		$("#LSsecondlabel").append("<option value='" + $(this).val() + "'>" + $(this).val() + "</option>");
	});
	LSselected_raws = [];
}

var onLSbackclick = function()
{
	//Reset the border colors back to their initial value
	setItemAttrColor("#LSfirstlabel", "border", "#DDDDDD");
	setItemAttrColor("#LSsecondlabel", "border", "#DDDDDD");
	setItemAttrColor("#LSbrep", "border", "#DDDDDD");
	setItemAttrColor("#LStrep", "border", "#DDDDDD");
	setItemAttrColor("#LSLabelSwaps", "border", "#DDDDDD");
}
var onLSrepchange = function()
{
	if(!AllowLS) return;
	//get all raw files corresponding the selected brep and trep
	 LSselected_raws = [];
	 var my_selectedbrep = $("#LSbrep").val();
	 var my_selectedtrep = $("#LStrep").val();
	 $("#LSRawFilesList").empty();
	 $.each(rawfiles_structure, function (idx, my_raw_file)
	 {
		 if (my_raw_file.used == false) return;
		 if (my_raw_file.biorep == my_selectedbrep && my_raw_file.techrep == my_selectedtrep)
		 {
			 LSselected_raws.push(my_raw_file.rawfile);
		 }
	 });
	 if(!(LSselected_raws.length < 1))
	 {
		 $.each(LSselected_raws, function (idx, my_raw_file)
		 {
			$("#LSRawFilesList").append("<option value='" + my_raw_file + "'>" + my_raw_file + "</option>");
		 });
	 }
}

var onLSlabelchange = function()
{	
	if(!AllowLS) return;
	var myfirstlabel = $("#LSfirstlabel").val();
	var myseclabel = $("#LSsecondlabel").val();
	if (myfirstlabel == myseclabel)
	{
		setItemAttrColor("#LSfirstlabel", "border", "#E60000");
		setItemAttrColor("#LSsecondlabel", "border", "#E60000");
	}
	else
	{
		setItemAttrColor("#LSfirstlabel", "border", "#DDDDDD");
		setItemAttrColor("#LSsecondlabel", "border", "#DDDDDD");
	}
}

var onLSrepclick = function()
{
	if(!AllowLS) return;
	setItemAttrColor("#LSbrep", "border", "#DDDDDD");
	setItemAttrColor("#LStrep", "border", "#DDDDDD");
}

var onLSLabelSwapsclick = function()
{
	if(!AllowLS) return;
	setItemAttrColor("#LSLabelSwaps", "border", "#DDDDDD");
}

var onLSAddclick = function()
{
	if(!AllowLS) return;
	var proc_failed = false;
	var myfirstlabel = $("#LSfirstlabel").val();
	var myseclabel = $("#LSsecondlabel").val();
	if (myfirstlabel == myseclabel)
	{
		setItemAttrColor("#LSfirstlabel", "border", "#E60000");
		setItemAttrColor("#LSsecondlabel", "border", "#E60000");
		proc_failed = true;
	}
	if (LSselected_raws.length < 1 )
	{
		setItemAttrColor("#LSbrep", "border", "#E60000");
		setItemAttrColor("#LStrep", "border", "#E60000");
		proc_failed = true;
	}
	if (proc_failed) return;
	var my_desc = myfirstlabel + " - " + myseclabel + " in b" + $("#LSbrep").val() + "t" + $("#LStrep").val();
	var shifted_desc = myseclabel + " - " + myfirstlabel + " in b" + $("#LSbrep").val() + "t" + $("#LStrep").val();
	var found_duplicate = false;
	$("#LSLabelSwaps option").each(function(idx, my_item){	
		if ($(this).val() == my_desc || $(this).val() == shifted_desc)
		{
			$(this).prop("selected", true);
			found_duplicate = true;
		}
		else
		{
			$(this).prop("selected", false);
		}
	});
	if (found_duplicate)
	{
		setItemAttrColor("#LSLabelSwaps", "border", "#E60000");
		return;
	}
	var ArrayOfIndices = [];
	 $.each(LSselected_raws, function (idx, my_raw_file)
	 {
		 ArrayOfIndices.push(LS_array_counter);
		 var toadd = [];
		 toadd[0] = my_raw_file;
		 toadd[1] = myfirstlabel;
		 toadd[2] = myseclabel;
		 toadd[3] = LS_array_counter++;
		 LabelSwapArray.push(toadd);
	 });
	 var to_add2 = [];
	 to_add2[0] = my_desc;
	 to_add2[1] = ArrayOfIndices;
	 to_add2[2] = [myfirstlabel, myseclabel];
	 LS_counters_per_Add.push(to_add2);
	 console.log(LabelSwapArray);	
	 $("#LSLabelSwaps").append("<option value='" + my_desc + "'>" + my_desc + "</option>");
	 setItemAttrColor("#LSLabelSwaps", "border", "#DDDDDD");
}
var onLSRemoveclick = function()
{
	if(!AllowLS) return;
	var my_counters = [];
	var options_to_erase = []; // to refresh the lslabelswaps list
	$("#LSLabelSwaps option:selected").each(function(idx, my_opt){
		$.each(LS_counters_per_Add, function(idx, LS_counters_val)
		{
			if(LS_counters_val[0] == $(my_opt).val())
			{
				// console.log("about to erase: " + $(my_opt).val());
				options_to_erase.push($(my_opt).val());
				$.each(LS_counters_val[1], function(idx, my_cval)
				{
					my_counters.push(my_cval);
				})
			}
		});
	});
	for(var i = LabelSwapArray.length - 1; i >= 0;i--)
	{
		if($.inArray(LabelSwapArray[i][3], my_counters) != -1)
		{
			LabelSwapArray.splice(i, 1);
		}
	}
	// console.log("LSArray after :");
	// console.log(LabelSwapArray);
	
	//Now refresh the lslabelswaps
	options_that_remain = [];
	// console.log(options_to_erase);
	$("#LSLabelSwaps option").each(function(idx, my_option)
	{
		if($.inArray($(my_option).val(), options_to_erase) == -1)
		{
			console.log($(my_option).val());
			options_that_remain.push($(my_option).val());
		}
	});
	$("#LSLabelSwaps").empty();
	$.each(options_that_remain, function(idx, my_option)
	{
		$("#LSLabelSwaps").append("<option value='" + my_option + "'>" + my_option + "</option>");
	});
	setItemAttrColor("#LSLabelSwaps", "border", "#DDDDDD");
}
var onlistclick = function()
{
	setItemAttrColor("#conditions_list", "border", "#DDDDDD");
}
var onexpquantfiltlblclick = function()
{
	setItemAttrColor("#expquantfiltlbl", "border", "#DDDDDD");
}
var inputChCheck = function (e, repatt, maxCharacters) {
   var theEvent = e || window.event;
   var key = theEvent.keyCode || theEvent.which;
   var re = new RegExp(repatt);
   var srcelem = e.target || e.srcElement;
   var txt = srcelem.value + String.fromCharCode(e.which);
   if (key === 8) {
      return;
   }

   if (!re.test(txt) || txt.length > maxCharacters) {
      e.preventDefault();
   }

}

var SwitchLFQList = function(e) {
   var theEvent = e || window.event;
   var srcelem = e.target || e.srcElement;
    $("#s2LFQConditionsList").attr("disabled", srcelem.value != "");
}

var onVIDDiscard_click = function()
{
	dlgFadeoutInfo();
}

var msgbox = function(displaytext)
{
	$(".expparamsDlgInfo").css({"left": ($("body").width() / 2) - ($("#VariousInfoDialog").width() / 2)});
	$('body').append('<div id="maskInfo"></div>');
	$("#VariousInfoDialog").fadeIn(300);
	$('#maskInfo').fadeIn(300);
	$("#InfoDiv").empty();
	$("#InfoDiv").append("<span>" + displaytext + "</span>");
}
// from http://codereview.stackexchange.com/questions/7001/better-way-to-generate-all-combinations
var combinations = function (str) {
   var fn = function (active, rest, a) {
      if (!active && !rest)
         return;
      if (!rest) {
         a.push(active);
      } else {
         fn(active + rest[0], rest.slice(1), a);
         fn(active, rest.slice(1), a);
      }
      return a;
   }
   return fn("", str, []);
}

var combinations_arr = function (arr, delim) {
   var ret = [];
   var str = "0123456789";
   var combs = combinations(str.slice(0, arr.length));
   for (var i in combs) {
      var comb_i = combs[i];
      var ret_i = "";
      ret_i = arr[str.indexOf(comb_i[0])];
      for (var i = 1; i < comb_i.length; i++) {
         ret_i = ret_i + delim + arr[str.indexOf(comb_i[i])];
      }
      ret.push(ret_i);
   }
   return ret;
}

// From an array of items matched by "selector", choose the one with index "idx" and set some attribute(s)
var helper_setItemArrAttr = function (selector, idx, json) {
   $($(selector).get(idx)).attr(json);
}

// For a single item matched by "selector" set some attribute(s)
var helper_setItemAttr = function (selector, json) {
   var x = $(selector).attr(json);
}

// Adds/Removes ("_removeClass" false/true) class "className" in succession (each time its called) for a list of items.
// Assumes ONLY ONE item in the list "itms" SHOULD/SHOULD NOT ("_removeClass" false/true) have the class "className".
// toggleFun executes code with the current (0-based) index as a parameter.
var toggleNextClass = function (itms, className, _removeClass, toggleFun) {
   var nToggle = 0;
   var toggleClassFunction = (_removeClass ? ["removeClass", "addClass"] : ["addClass", "removeClass"]);
   var reachedLastItem = false;
   itms.some(function (itm, index) {
      var cond = (_removeClass ? $(itm).hasClass(className) : !$(itm).hasClass(className));
      if (cond) {
         if (nToggle > 0) {
            $(itm)[toggleClassFunction[0]](className);
            nToggle++;
         }
      } else {
         if (toggleFun != null) {
            if (toggleFun(index)) {
               $(itm)[toggleClassFunction[1]](className);
               nToggle++;
            }
            ;
         } else {
            $(itm)[toggleClassFunction[1]](className);
            nToggle++;
         }
      }
      return (nToggle === 2);
   });
   // Toggle 1st item again if toggled all in succession (valid when we have more than one items in the list)
   if (nToggle == 1 && itms.length > 1) {
      var itm = itms[0];
      var cond = (_removeClass ? $(itm).hasClass(className) : !$(itm).hasClass(className));
      if (cond) {
         $(itm)[toggleClassFunction[0]](className);
      } else {
         $(itm)[toggleClassFunction[1]](className);
      }
   }
}

// Get array of items from the DOM using a selector and id pattern
var getItems = function (className, id_pattern) {
   return $(className).toArray().filter(function (element) {
      var match;
      if($(element).attr("id")){
         match = ($(element).attr("id")).match(id_pattern);
      }else{
         match = null;
      }
      return (match == null ? false : true);
   });
}

// Get an array of items IDs' strings (starting with a #)
var idsToStrings = function (itemsSelector) {
   return $.map($(itemsSelector).toArray(), function (itm, i) {
      return "#" + $(itm).attr("id");
   });
}

// Called by "toggleNextClass". It is used for hiding the "Previous" button when we are at stage 1, i.e. there is no previous stage.
var nextStageIsFirstStage = function (lastStageItem) {
   $("#s1btnb").addClass("hidden");
}

// Reset styles based on source CSS (i.e. discard changes made through JS)
var resetCssStyles = function (selector) {
   $(selector).removeAttr("style");
}

// Self-explanatory
var setItemAttrColor = function (selector, attr, hexcolor) {
   var cssstr = $(selector).css(attr);
   if (cssstr.length == 0) { // FireFox (!)
      cssstr = $(selector).css(attr + "-top-color");
      $(selector).css(attr + "-top-color", cssstr.replace(/rgb\([0-9]+, [0-9]+, [0-9]+\)$/g, hexcolor));
      cssstr = $(selector).css(attr + "-right-color");
      $(selector).css(attr + "-right-color", cssstr.replace(/rgb\([0-9]+, [0-9]+, [0-9]+\)$/g, hexcolor));
      cssstr = $(selector).css(attr + "-bottom-color");
      $(selector).css(attr + "-bottom-color", cssstr.replace(/rgb\([0-9]+, [0-9]+, [0-9]+\)$/g, hexcolor));
      cssstr = $(selector).css(attr + "-left-color");
      $(selector).css(attr + "-left-color", cssstr.replace(/rgb\([0-9]+, [0-9]+, [0-9]+\)$/g, hexcolor));
   } else {
      $(selector).css(attr, cssstr.replace(/rgb\([0-9]+, [0-9]+, [0-9]+\)$/g, hexcolor));
   }
}

var sectionSpinnerOn = false;
var sectionSpinnerText = "";

var toggleCurrentSectionSpinner = function() {
	if(sectionSpinnerOn)
	{
		$('.main_div .main_section:not(.hidden) h2').html(sectionSpinnerText);
	}
	else
	{
		sectionSpinnerText = $('.main_div .main_section:not(.hidden) h2').text();
		$('.main_div .main_section:not(.hidden) h2').html(sectionSpinnerText + " <i class='fa fa-cog fa-spin'></i>");
	}
	sectionSpinnerOn = !sectionSpinnerOn;
}

// Clear areas where results information appears.
var resetResultsInfoItms = function () {
   $("#server_feedback").empty();
   $("#dndres").empty();
   $("#results_p").html("Now analysing your data. Please wait for the results.");
}

var postFireUpAnalysisAndWait = function () {
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   thedata.append("AllowMergeLabels", AllowMergeLabels ? "T" : "F");
   thedata.append("AllowLS", AllowLS ? "T" : "F");
   if(!sectionSpinnerOn)
   {
	   toggleCurrentSectionSpinner();
   }
   $("#s4btnf").prop('disabled', true);
   $.ajax({
      url: cgi_bin_path + 'perform_analysis.php', //Server script to fire up data analysis
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false,
      beforeSend: function (jqXHR, settings) {
         //fire-up spinner
         //$("#server_feedback").html("<div class='loadingclockcontainer'><div class='box'><div class='clock'></div></div></div>");
         getRSS("http://www.nature.com/nmeth/current_issue/rss", "#server_feedback");
      },
   }).done(function (data, textStatus, jqXHR) {
	  if (analysis_finished | data.ret_session != sessionid)
	  {
		  return;
	  }
	  var R_returned_error = false;
	  $("#s4btnb").prop('disabled', true);
	  if(sectionSpinnerOn)
	  {
		toggleCurrentSectionSpinner();
	  }
      $("#server_feedback").empty();
      $("#s4btnf").prop('disabled', !data.success);
	  var my_UserInfo = data.UserInfo;
	  var UserInfoDisplay = "";
	  if(typeof(my_UserInfo) != "undefined" && my_UserInfo != "")
	  {
		  	var lines = my_UserInfo.split("\t\t");
			$.each(lines, function(idx, my_line){
				UserInfoDisplay = UserInfoDisplay + my_line.replace(/\[.*?\]/i, "</p>&#13");
			});
	  }
	  if (UserInfoDisplay != "")
	  {
		  UserInfoDisplay = UserInfoDisplay.replace("Warn User: ", '<p style="color: #BA4A00">')
		  UserInfoDisplay = UserInfoDisplay.replace("Error User: ", '<p style="color: #E60000">')
		  UserInfoDisplay = UserInfoDisplay.replace("Info User: ", '<p>')
		  $("#s4UserInfoText").empty();
		  $("#s4UserInfoText").append(UserInfoDisplay);
		  $(".expparamsDlg").css({"left": ($("body").width() / 2) - ($("#s4UserInfo").width() / 2)});
		  $('body').append('<div id="mask"></div>');
		  $("#s4UserInfo").fadeIn(300);
		  $('#mask').fadeIn(300);
		  var lbl = $(this);
		  $("#s4UserInfoOK").unbind();
		  $("#s4UserInfoOK").on("click", function () {
			 dlgFadeout();
			 //ADD OK RESULT HERE
		  });
	  }
      if (data.success) {
         $("#results_p").html("Now you can inspect your results. When ready, click <em>Next</em>.");
		 //WIN TODO: \\ in next line
         $("#dndres").html("<span><a href=" + data.results_url + "><strong>" + data.results_url.substr(data.results_url.lastIndexOf("/") + 1) + "</strong></a></span>");
         patt = new RegExp("limma\-graphs");
         $.each(data.results_preview, function (idx, path_to_img_i)
         {
			//WIN TODO: \\ in next line
            var img_i = path_to_img_i.substr(data.results_url.lastIndexOf("\\") + 1);
            if (!patt.test(img_i)) {
               $("#server_feedback").append("<div class='resimg'><a href='" + path_to_img_i + "' target='_blank'><img src='" + path_to_img_i + "' width='120'></img></a></div>");
            }
         });
      } else {
         $("#results_p").html("");
         $("#server_feedback").html("<span class='uploadErrorMsg'><strong><em>The analysis could not be completed: " + data.msg + "<em><strong></span>");
         if (data.R_dump.length > 0) {
            $("#server_feedback").append("<br><br><span style='font-family: Georgia; font-size: 95%; text-align: left; display: inline-block; width: 90%'><p>Please ensure that input parameters (such as number of replicates, number of biological conditions/labels etc) are correctly defined and input data format is valid. The statistical analysis routine relies heavily on the validity of the input parameters.</p><p>If the above does not apply, then the statistical analysis may have failed due to numerical problems (e.g. there were too many missing values/data points).</p><p> If you feel that none of the above is the case, please click <a href='mailto:msdiffexp@gmail.com?Subject=Session%20" + sessionid + "' target='_blank'><u>here</u></a> to notify via e-mail (do not delete the session id in the subject) the ProteoSign team for investigation of your analysis issue.</p></span>")
            //$("#server_feedback").append("<br><br><span style='font-family: \"Courier New\"'>Logged information:</span><br>");
            //$("#server_feedback").append("<div style='height: inherit'><textarea style='font-family: \"Courier New\"; font-size: 90%; width: 90%; height: 100%' readonly>"+ data.dump +"</textarea></div>");
         }
		 R_returned_error = true;
		 $("#s4btnb").prop("disabled", false);
      }
	  if (!R_returned_error) analysis_finished = true;
   }).fail(function (jqXHR, textStatus, errorThrown) {
   	toggleCurrentSectionSpinner();
      $("#server_feedback").empty();
      $("#server_feedback").html("<span class='uploadErrorMsg'><strong><em>An AJAX error occurred: " + errorThrown + "<em><strong></span>");
   });
}

var postClientServerClientInfo = function () {
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   $.ajax({
      url: cgi_bin_path + 'get_serverclieninfo.php',
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false,
      beforeSend: function (jqXHR, settings) {
      }}).done(function (data, textStatus, jqXHR) {
      clientname = data.hostname;
      softversion = data.version;
      $("#scrollingtext").html("Welcome <em>" + clientname + "</em> to ProteoSign");
      $("#proteosignversion").html("ProteoSign version " + softversion);
   }).fail(function (jqXHR, textStatus, errorThrown) {
   });
}

function dumpExpParamSQL(tmp){
   //Dump to a .sql the exp. params for DB import
   var sqlscript = [];
   var expdesc = $('input[name="expid"]').val();
   sqlscript.push("insert into dataset (desc) values ('" + expdesc + "');");
   var filesvaluesstr = $.map(uploadingFiles, function(i, v){return i.name;}).join("','");
   $.each(uploadingFiles, function (i, v){
      sqlscript.push("insert into files (file) values ('" + v.name + "');");
   });   
   var procfilesvaluesstr = $.map(rawfiles_structure, function(i, v){ return i.rawfile; }).join("','");
   $.each(rawfiles_structure, function (i, v){
      sqlscript.push("insert into processed_files (name) values ('" + v.rawfile + "');");
   });
   sqlscript.push("insert into dataset_files (dataset_id, file_id) select (select id from dataset where desc = '" + expdesc + "') as dataset_id, id as file_id from files where file in ('" + filesvaluesstr + "');");
   $.each(tmp, function (key, val){ 
      sqlscript.push("insert into param_value (param_id,value,dataset_id) select id,'" + val + "' as value,(select id from dataset where desc = '" + expdesc + "') as dataset_id from param where selector like '%" + key + "%';");
   });
   $.each(rawfiles_structure, function (key, val){ 
      sqlscript.push("insert into experimental_structure (processed_file_id,brep,trep,frac,dataset_id) values ('" + val.rawfile + "', '" + val.biorep + "', '" + val.techrep + "', '" + val.fraction + "', '0');");
   });
   sqlscript.push("update experimental_structure set dataset_id select id from dataset where dataset.desc = '" + expdesc + "' and experimental_structure.dataset_id = 0;");
   sqlscript.push("update experimental_structure set processed_file_id select id from processed_files where processed_files.name in ('" + procfilesvaluesstr + "') and experimental_structure.processed_file_id = 0;");
   //
   var link = document.getElementById('github');
   //link.href = URL.createObjectURL(new Blob([sqlscript.join("\n")], {type: 'text/plain'}));
   //link.click();
}

var postParameters = function (params) {
   var tmp = {};
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   $.each(params, function (idx, param_i)
   {
      var theval = $(param_i).val();
      switch ($(param_i).attr('type')) {
         case "checkbox":
            theval = ($(param_i).prop("checked") ? 1 : 0);
            // Here the on/off is transmitted Yes/No
            thedata.append($(param_i).attr('name'), (theval ? "T" : "F"));
            break;
         default:
            if (theval == null) {
               theval = '';
            }
			// console.log($(param_i).attr('name'));
			if ($(param_i).attr('name') == "conditions_list")
			{
				var mytmp = "#" + $(param_i).attr('id') +  " option:selected";
				var tmp_i = 0;
				$(mytmp).map(function () {
					if (!AllowMergeLabels)
					{
						thedata.append("explbl" + (tmp_i + 1) + "name", $(this).text());
						thedata.append("explbl" + (tmp_i + 1) + "def", "");
						// console.log("each at 354 " + $("#explbl" + (tmp_i + 1) + "name_").val());
						tmp_i++;	
					}
					else
					{
						var my_temp_text = $(this).text();
						if($.inArray(my_temp_text, AuthenticItemsinRename) != -1)
						{
							thedata.append("explbl" + (tmp_i + 1) + "name", my_temp_text);
							thedata.append("explbl" + (tmp_i + 1) + "def", "");
							// console.log("each at 354 " + $("#explbl" + (tmp_i + 1) + "name_").val());
							tmp_i++;
						}
						else
						{
							 $.each(RenameArray, function(idx, my_row)
							 {
								 if(my_row[1] == my_temp_text)
								 {
									thedata.append("explbl" + (tmp_i + 1) + "name", my_row[0]);
									thedata.append("explbl" + (tmp_i + 1) + "def", "");
									tmp_i++;
								 }
							 });						
						}
	
					}

				});
			}
			else
			{
				thedata.append($(param_i).attr('name'), theval);
			}
            break;
      }
      if($(param_i).attr('id')){
         tmp[$(param_i).attr('id')] = theval;
      }else{
         tmp[$(param_i).attr('name')] = theval;
      }
   });
	// for (var pair of thedata.entries()) {
		// // console.log(pair[0]+ ', ' + pair[1]); 
	// }
   dumpExpParamSQL(tmp);
   thedata.append("labelfree", ((peptideLabelsNamesFromFile.length == 0 && peptideLabelsFromFile.length > 0) ? 'T' : 'F'));
   thedata.append("AllowMergeLabels", AllowMergeLabels ? "T" : "F");
   thedata.append("AllowLS", AllowLS ? "T" : "F");
   thedata.append("exp_struct", gen_expdesign(rawfiles_structure));
   thedata.append("LFQ_conds", gen_lfqdesign(RawFileConditions));
   if(AllowMergeLabels) thedata.append("Rename_Array", gen_RenameFile(RenameArray));
   if(AllowLS) thedata.append("LabelSwapArray", gen_LSArray(LabelSwapArray));
   thedata.append("IsIsobaricLabel", isIsobaricLabel ? "T" : "F");
   thedata.append("All_MQ_Labels", my_all_mq_labels);
   $.ajax({
      url: cgi_bin_path + 'upload_parameters.php', //Server script to receive parameters
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false
   }).done(function (data, textStatus, jqXHR) {
//if there was a server-side error alert.
      if (!data.success) {
         alert("ERROR on SERVER: " + data.msg);
      } else {
         postFireUpAnalysisAndWait();
      }
   });
}

// Executed when a "Next" type of button is clicked
var executeStage = function (stageIndex) {
   var ret = true;
   switch (stageIndex) {
      case 1:
         if(tour_mode){
            onShowDialog("#tourModeDlg1");
         }
         break;
      case 2:
         if(tour_mode){
            onShowDialog("#tourModeDlg2");
         }
         break;
      case 3:
         var parameterObjs = $("#s3expparams input,#s3expparams select");
         if (validateParameters(parameterObjs)) {
            postParameters(parameterObjs);
            // Clear areas where results information appears.
            resetResultsInfoItms();
         } else {
            ret = false;
         }
         break;
      case 5:
         resetState();
		 window.location.reload();
         break;
      default:
   }

   return ret;
}
// Executed when a "Back" type of button is clicked
var rollbackStage = function (stageIndex) {
   var ret = true;
   stageIndex = getItems("button.main", /s[0-9]+btnb/).length - stageIndex - 1;
   switch (stageIndex) {
    case 1:
         resetState();
	case 3:
		 resetSession();
      default:
   }   
   return ret;
}

var resetSession = function ()
{
   var thedata = new FormData();
   thedata.append('old_session_id', sessionid);
	sessionid = new Date().getTime();
	thedata.append('session_id', sessionid);
	$.ajax({
		url: cgi_bin_path + 'change_session.php', //Server script to fire up data analysis
		type: 'POST',
		// Form data
		data: thedata,
		//Options to tell jQuery not to worry about content-type.
		processData: false,
		cache: false,
		contentType: false
	}).done(function (data, textStatus, jqXHR) {});
}

var unsuccessfullyUploadedFiles = {};
var uploadEfforts = {};
var peptideLabelsFromFile = [];
var peptideLabelsNamesFromFile = [];
var peptideLabelsFromFileCombs = [];
var nFormLabels = 1;
var nToUpload;
var debug_ajax_data;
var tour_mode = false;
// Return false if at least one required parameter (existence of data-required attr) is not set (empty text field)
// Mark invalid parameters with a red border color (currently tested on input fields)
// Marked fields are reset through the 'focusout' event (see respective binding in function document.ready)
var validateParameters = function (params) {
   var nValid = 0;
   var nInvalid = 0;
   $.each(params, function (idx, param_i)
   {
      switch ($(param_i).attr('type')) {
         case "text":
            if ($(param_i).attr('data-required') != null && $(param_i).attr('data-required')) {
               if ($(param_i).val() == "" && !$(param_i).hasClass("hidden")) {
                  nInvalid++;
                  setItemAttrColor(param_i, "border", "#E60000");
               }
            }
            break;
         default:
            nValid++;
            break;
      }
   });
	var mytmp = "#conditions_list option:selected";
	var tmp_i = 0;
	$(mytmp).map(function () {
		tmp_i++;
	});
	if (tmp_i <= 1)
	{
		setItemAttrColor("#conditions_list", "border", "#E60000");
	}
	var found_cond = false;
	if ($("#expquantfilt").prop("checked") == true)
	{
		var my_text = $("#expquantfiltlbl option:selected").text();
		$("#conditions_list > option:selected").each(function() {
			if (this.text == my_text)
			{
				found_cond = true;
			}
		});
		if (found_cond == false)
		{
			setItemAttrColor("#expquantfiltlbl", "border", "#E60000");
		}
	}
	else
	{
		found_cond = true;
	}

   return (nInvalid == 0 && tmp_i > 1 && found_cond);
}

// reset "counters"/states reset state(false) is called whenever the program starts from the very beggining
var resetState = function (uploading_new_file = false) {
   unsuccessfullyUploadedFiles = {};
   uploadEfforts = {};
   if (!$("#explbl1definition").hasClass("hidden")) {
      $("#explbl1definition").addClass("hidden");
   }
	if(uploading_new_file == false)
	{
		// Reset items that contained information from previous data input files (e.g. label information)
		$("#s3expparamsDlgLabelsSelection").empty();
		$("#s3div h2").html("Step 3 ");
		$("#explbl1name_").empty();
		$("#explbl0name_").empty();
		$("#s3advparams select[name='expquantfiltlbl']").empty();
		$("#quantsoftdetectedspan").empty();
		rawfiles = undefined;
		procProgram = "";
		peptideLabelsFromFile = [];
		peptideLabelsFromFileCombs = [];
		peptideLabelsNamesFromFile = [];
		$("#s2uluploaders > table").empty();
		sessionid = new Date().getTime();
		uploaded_files = 0;
		upload_was_successful = false;
		$("#s2btnf").prop('disabled', true);
		types_of_files_uploaded = [];
		nToUpload = 0;
		AddedLabels = false;
		RawFileConditions = [];
		isLabelFree = false;
		LFQconditions = [];
		isIsobaricLabel = false;
		RenameFromTestData = false;
		LabelSwapArray = [];
		LSselected_raws = [];
		LS_counters_per_Add = [];
		my_all_mq_labels = "";
		itemsToRename = [];
		AuthenticItemsinRename = [];
		RenameArray = [];
		AppendNewLabels = false;
		analysis_finished = false;
		datatestOK_clicked = false;
		tmp_nToUpload = 0;
		my_lbls_toselect = [];

	}
	
   nUploaded = uploaded_files;
   tour_mode = false;
}

// "uploadingFiles": Files (array returned by FileChooser) selected for upload in stage #2
var uploadFiles = function (serverSide, postSuccess) {
   resetState(true);
   tour_mode = serverSide;
   $("#s2btnf").prop('disabled', true);

   $.each(uploadingFiles, function (idx, file_i) {
	  var temp_index = uploaded_files + nToUpload + idx;
      $("#s2uluploaders table").append("<tr><td>" + file_i.toString() + "</td><td><progress max='100' value='0' id=uploadfile" + temp_index + "><div class='progress-bar'><span style='width: 80%;'></span></div></progress></td></tr>");
	  // console.log("	created table row for file: " + temp_index + file_i.toString());
   });
	// fire AJAX calls
   $.each(uploadingFiles, function (idx, file_i)
   {
	  var temp_index = uploaded_files + nToUpload;
      postFile(temp_index, file_i, serverSide, postSuccess);
	  nToUpload++;
	  tmp_nToUpload++;
	  // console.log("	fired post file for file: " + temp_index);
   });
  }

// Called by respective AJAX event handlers (see postFile function below)
var uploadFinished = function (success, idx, file) {
	// console.log("	Upload Finished called for idx: " + idx + "nToUpload: " + nToUpload);
	nToUpload = nToUpload - 1;
	uploaded_files++;
	// console.log("	uploaded files updated: " + uploaded_files);
   uploadEfforts[idx] = file;
   if (!success) {
	  // console.log("unsuccessfullyUploadedFiles != 0");
      unsuccessfullyUploadedFiles[idx] = file;
   }
}

// jQuery AJAX POST for uploading a single file
var postFile = function (idx, file, serverSide, postSuccess) {
   var thedata = new FormData();
   thedata.append('thefile', file);
   thedata.append('session_id', sessionid);
   thedata.append('server_side', serverSide);
   var progresstditm = $("#s2uluploaders table tr:nth-child(" + (idx + 1) + ") td").get(1);
   $.ajax({
      url: cgi_bin_path + 'upload_files.php', //Server script to receive file
      type: 'POST',
      xhr: function () {  // Custom XMLHttpRequest
         var myXhr = $.ajaxSettings.xhr();
         if (myXhr.upload) { // Check if upload property exists
            myXhr.upload.addEventListener('progress', function (e) {
               if (e.lengthComputable) {
                  if (e.loaded / e.total == 1.0) {
                     $(progresstditm).html("<span class='uploadSuccessMsg'><strong><em>Processing, please wait ...<em><strong></span>");
                     toggleCurrentSectionSpinner();
                  } else {
                     helper_setItemAttr("#uploadfile" + idx, {value: e.loaded, max: e.total});
					 // console.log("#uploadfile" + idx + " new prog: " + e.loaded + "/"  +  e.total);
					 // console.log("		#uploadfile" + idx + "value " + $("#uploadfile" + idx).attr("value") + "max: " + $("#uploadfile" + idx).attr("max"));
                  }
               }
            }, false); // For handling the progress of the upload
         }
         return myXhr;
      },
      // Form data
      data: thedata,
      //Options to tell jQuery not to process data or worry about content-type.
      cache: false,
      contentType: false,
      processData: false,
      //Ajax events
      beforeSend: function (jqXHR, settings) {
         if (serverSide) {
            helper_setItemAttr("#uploadfile" + idx, {value: 0, max: 100});
         }
      }}).done(function (data, textStatus, jqXHR) {
      	if(sectionSpinnerOn)
	      	toggleCurrentSectionSpinner();
//debug_ajax_data = data;
//If server-side everything went fine (internal things that the server had to do with the client's file, such as storage etc)
      uploadFinished(data.success, idx, file);
      //if everything went fine enable button for next stage and print OK. Just print the error message otherwise.
      if (data.success) {
types_of_files_uploaded.push(data.file_type);
		  if (data.msg == "")
		  {
			$(progresstditm).html("<span class='uploadSuccessMsg'><strong><em>OK<em><strong></span>");
		  }
		  else
		  {
			$(progresstditm).html("<span class='uploadWarningMsg'><strong><em>" + data.msg + "<em><strong></span>");
		  }
		  var tmp_rawfiles = [];
         for (var i = 0; i < data.raw_filesnames.length; i++) {
            tmp_rawfiles.push(data.raw_filesnames[i]);
         }
// If we have info regarding rawdata files names, save it					
         if (tmp_rawfiles.length > 0) {
            rawfiles = tmp_rawfiles;
            reset_reps();
         }
// If we have info regarding peptide labels and such info has not been available in previous upload
         if (peptideLabelsNamesFromFile.length == 0 && (data.peptide_labels_names.length > 0 || data.peptide_labels.length > 0)) {
            peptideLabelsFromFile = data.peptide_labels.sort();
            peptideLabelsNamesFromFile = data.peptide_labels_names.sort();
            // some doc items that depend on whether the experiment is label-free or not
            // 1. Non-labelled "background" species present?
            var item1 = $("#s3advparams input[name='explbl00']").closest("tr").children().first();
            // 2. Quantitation filtering?
            var item2 = $("#s3advparams input[name='expquantfilt']").closest("tr").children().first();
            // 3. Species label #1
            var item3 = $("#explbl1name_").closest("tr").children().first();
            // 4. Species label #1 select item td
            var item4 = $("#explbl1name_").closest("td");
            // 5. Label tooltip
            var item5 = $("#quantsoftdetectedspan").closest("td").children().first();
            // 6. Species label #1 definition
            var item6 = $("#explbl1definition");
            // if not label-free data
            if (peptideLabelsNamesFromFile.length > 0) {
// Set states of parameters relevant to labelled experiments accordingly
               $(item1).prop('disabled', false);
               $(item2).prop('disabled', false);
               $(item3).html('&#8212 Species label #1');
               $(item4).html('<select data-required="true" id="explbl1name_" name="explbl1name" type="text" placeholder="Name"></select>');
               // Only Proteome Discoverer data currently provide label definition information.
               // When this information is made available it means that our data originate from PD software.
               $("#s3expparams input[name='exppddata']").prop('checked', peptideLabelsFromFile.length > 0);
               //<img class="callout" src="../images/callout_black.gif" /><strong>Warning!</strong><br><u>The order of labels defined here matters</u>. Define your labels in the same order they were defined in <a id="quantsoftdetectedspan"></a>. If there exist unlabeled species, please define them in the <em>advanced parameters</em> section below.
               $("#quantsoftdetectedspan").text(peptideLabelsFromFile.length > 0 ? "Proteome Discoverer" : "MaxQuant");
               $("#quantitation_prog_lbl").text(peptideLabelsFromFile.length > 0 ? "\u2014 Raw data were quantified with Proteome Discoverer\u2122" : "\u2014 Raw data were quantified with MaxQuant");
               $(item5).removeClass('hidden');
               $(item6).attr('placeholder', 'Definition');
			   $(document).contextmenu("showEntry", "assign_condition", false);
			   $("#ExtraInfoForLFQ").hide();
			   //Add here more functionality in case of Labeled data
			   rawfiles_tbl_allfiles_DT.column("4").visible(false);
			   isLabelFree = false;
			   
            } else {
//label-free case (disable non-applicable parameters and rename others accordingly)
			   isLabelFree = true;
               $(item1).prop('disabled', true);
               $(item2).prop('disabled', true);
               $(item3).html('&#8212 Biological condition #1');
               $(item4).html('<input data-required="true" id="explbl1name_" name="explbl1name" type="text" onkeypress="inputChCheck(event,\'^(?!_)[a-zA-Z0-9_]+$\',20)" placeholder="Character rules apply"></input>');
               $(item5).addClass('hidden');
               $(item6).attr('placeholder', 'Raw file');
			   $("#ExtraInfoForLFQ").show();
			   $(document).contextmenu("showEntry", "assign_condition", true);
			   rawfiles_tbl_allfiles_DT.column("4").visible(true);
			   //Add here more functionality in case of LFQ data
			   
            }

            // $.each(peptideLabelsFromFile, function (idx, lbl_i) {
			   // $("#conditions_list").append("<option value='" + lbl_i + "' style='padding: 2px; font-size: 125%;' selected='true'>" + lbl_i + "</option>");
               // $("#s3expparamsDlgLabelsSelection").append("<option value='" + lbl_i + "'>" + lbl_i + "</option>");
            // });
			if (AddedLabels == false)
			{
				if (AppendNewLabels == false)
			   {
				   $('#conditions_list').empty();
			   }
			   else
			   {
				   AppendNewLabels = false;
			   }
			   $.each(peptideLabelsNamesFromFile, function (idx, lblname_i) {
					// $("#conditions_list").append("<option value='" + lblname_i + "' style='padding: 2px; font-size: 125%;' selected='true'>" + lblname_i + "</option>");
				   // // $("#explbl1name_").append("<option value='" + lblname_i + "'>" + lblname_i + "</option>");
				   // // $("#explbl0name_").append("<option value='" + lblname_i + "'>" + lblname_i + "</option>");
				   $("#s3advparams select[name='expquantfiltlbl']").append("<option oncontextmenu='return false;' value='" + lblname_i + "'>" + lblname_i + "</option>");
               });
				addFormLabel();
			   AddedLabels = true;
			}

            if (peptideLabelsFromFile.length > 0) {
               $("#explbl1definition").removeClass("hidden");
            }
            while (nFormLabels > 1) {
               removeFormLabel();
            }
         }
      } else {
         $(progresstditm).html("<span class='uploadErrorMsg'><strong><em>A server-side error occurred: " + data.msg + "<em><strong></span>");
      }
      if (typeof (postSuccess) == "function") {
         postSuccess();
      }
   }).fail(function (jqXHR, textStatus, errorThrown) {
   	if(sectionSpinnerOn)
	   	toggleCurrentSectionSpinner();
      $(progresstditm).empty();
      $(progresstditm).html("<span class='uploadErrorMsg'><strong><em>An AJAX error occurred: " + errorThrown + "<em><strong></span>");
      uploadFinished(false, idx, file);
   });
}

var bind_explbldefinition_focus = function (explbldefinition) {
   $(explbldefinition).on("focus", function () {
      $(".expparamsDlg").css({"left": ($("body").width() / 2) - ($("#s3expparamsDlgLabels").width() / 2)});
      $('body').append('<div id="mask"></div>');
      $("#s3expparamsDlgLabels").fadeIn(300);
      //$('#mask').fadeIn(300);
      var lbl = $(this);
      $("#dlglabelsBtnOK").unbind();
      $("#dlglabelsBtnOK").on("click", function () {
         dlgFadeout();
         var selection = $("#s3expparamsDlgLabelsSelection").val();
         if (selection != null) {
            $(lbl).val(selection.join(","));
         }
      });
   });
}
var bind_s2LFQConditions_focus = function () {
      $(".expparamsDlg").css({"left": ($("body").width() / 2) - ($("#s3expparamsDlgLabels").width() / 2)});
      $("#s2LFQConditions").fadeIn(300);
      //$('#mask').fadeIn(300);
      var lbl = $(this);
      $("#s2LFQConditionsOK").unbind();
      $("#s2LFQConditionsOK").on("click", function () {
         dlgFadeout();
		 //ADD OK RESULT HERE
      });
}

var ons22btnfclick = function()
{
	if (isLabelFree)
	{
		//First clean the conditions from RawFileConditions that are not used
		RawFileConditions_copy = RawFileConditions.slice();
		$.each(rawfiles_structure, function (idx, my_raw_file)
		{
			if (my_raw_file.used == false)
			{//for each rawfile not used
				$.each(RawFileConditions_copy, function (idxC, my_cond)
				{
					if (my_raw_file.rawfile == my_cond.name)
					{//if there is a row in RawFileConditions_copy corresponding to a not used rawfile, erase this row
						RawFileConditions_copy.splice(idxC, 1);
						//normally there can not be two rows in RawFileConditions_copy so escape the loop if erased a row
						return false;
					}
				});
			}
		});
		$('#conditions_list').empty();
		var non_un_condiitions = RawFileConditions_copy.map(function(value, index){return value.condition;});
		var unique_conditions = ArrNoDupe(non_un_condiitions);
		$.each(unique_conditions, function (idx, my_un_cond)
		{
			$("#conditions_list").append("<option value='" + my_un_cond + "' style='padding: 2px; font-size: 125%;' selected='true'>" + my_un_cond + "</option>");
		});
		select_labels_according_to_test_dataset();
		if(AllowMergeLabels) InitializeRename();
		create_my_all_mq_labels();
	}
}
var onShowDialog = function (selector){
   $(".expparamsDlg").css({"left": ($("body").width() / 2) - ($(selector).width() / 2)});
   $('body').append('<div id="mask"></div>');
   $(selector).fadeIn(300);
   //$('#mask').fadeIn(300);   
}

var addFormLabel = function()
{
	nFormLabels++;
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   $.ajax({
      url: cgi_bin_path + 'get_labels_to_js.php',
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false
   }).done(function (data) {
		if (data.success && data.labels.length > 0) {
			// console.log(data.labels[0]);
			var methods_mismatch = false;
			for (var i = 0; i < data.labels.length; i++) {
				if(peptideLabelsNamesFromFile.indexOf(data.labels[i]) == -1)
				{
					methods_mismatch = true;
				}
			}
			if (methods_mismatch == true)
			{
				// console.log("Methods mismatch in rearranging maxquant labels!!");
			}
			peptideLabelsNamesFromFile = data.labels;
		}
		if (peptideLabelsNamesFromFile.length > 0) {
			if(!AllowMergeLabels)
			{
				$.each(peptideLabelsNamesFromFile, function (idx, lblname_i) {
					$("#conditions_list").append("<option value='" + lblname_i + "' style='padding: 2px; font-size: 125%;' selected='true'>" + lblname_i + "</option>");
				});
			}
			else{
				if (!RenameFromTestData)
				{
					$.each(peptideLabelsNamesFromFile, function (idx, lblname_i) {
						$("#conditions_list").append("<option value='" + lblname_i + "' style='padding: 2px; font-size: 125%;' selected='true'>" + lblname_i + "</option>");
					});
					InitializeRename();
				}
			}
			select_labels_according_to_test_dataset();
		  create_my_all_mq_labels();
		  //Inform ProteoSign if this file is a reporter-ion derived file
		  isIsobaricLabel = data.isIsobaricLabel;
		  $("#s3QuantitationFiltering").show();
	   } else {
	//label-free case
		  if (peptideLabelsFromFile.length > 0) {
				if(!AllowMergeLabels)
				{
				  $.each(peptideLabelsFromFile, function (idx, lblname_i) {
						$("#conditions_list").append("<option value='" + lblname_i + "' style='padding: 2px; font-size: 125%;' selected='true'>" + lblname_i + "</option>");
				  });
				}
				else
				{
					if(!RenameFromTestData)
					{
						$.each(peptideLabelsFromFile, function (idx, lblname_i) {
							$("#conditions_list").append("<option value='" + lblname_i + "' style='padding: 2px; font-size: 125%;' selected='true'>" + lblname_i + "</option>");
						});
						InitializeRename();
					}
				}
				select_labels_according_to_test_dataset();
			  create_my_all_mq_labels();
			  //hide the advanced parameters (Quantitation filtering) in case we have label-free data
			  $("#s3QuantitationFiltering").hide();
		  }
	   }
	   // label-free case
	   if (peptideLabelsNamesFromFile.length == 0 && peptideLabelsFromFile.length > 0) {
		  $("#btnAddLabel").prop("disabled", nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsFromFile.length - 2) : peptideLabelsFromFile.length - 1));
	   } else {
		  $("#btnAddLabel").prop("disabled", nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsNamesFromFile.length - 2) : peptideLabelsNamesFromFile.length - 1));
	   }
   });
}

var removeFormLabel = function () {
   nFormLabels--;
   var rembtntd = $("#btnRemLabel").closest("td");
   var addbtntd = $(rembtntd).prev();
   var last_tr_of_table = $("#btnRemLabel").closest("table").children().first().children().last();
   $(last_tr_of_table).prev().children().last().after($(addbtntd));
   $(last_tr_of_table).prev().children().last().after($(rembtntd));
   $(last_tr_of_table).remove();
   $("#btnRemLabel").prop("disabled", nFormLabels == 1);
   // label-free case
   if (peptideLabelsNamesFromFile.length == 0 && peptideLabelsFromFile.length > 0) {
      $("#btnAddLabel").prop("disabled", nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsFromFile.length - 2) : peptideLabelsFromFile.length - 1));
   } else {
      $("#btnAddLabel").prop("disabled", nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsNamesFromFile.length - 2) : peptideLabelsNamesFromFile.length - 1));
   }
   if (peptideLabelsFromFile.length > 0) {
      bind_explbldefinition_focus("#explbl" + nFormLabels + "definition");
   }
}

var downloadTestDataset = function (dataset_desc) {
   window.location = cgi_bin_path + 'download_test_data.php?session_id=' + sessionid + '&dataset_info_requested=' + dataset_desc;
}

function refresh_fractions(){
		//Now check that the fractions are set correctly
		//The algorithm is slightly different in label free and labeled cases
		if (isLabelFree == false)
		{
			$.each(rawfiles_structure, function (idx, my_raw_file)
			{
				if (my_raw_file.used == false) return;
				// console.log("Checking fractionation in " + my_raw_file.rawfile);
				var my_brep = my_raw_file.biorep;
				var my_trep = my_raw_file.techrep;
				var my_cur_fraction = 1;
				$.each(rawfiles_structure, function (idxJ, my_raw_fileJ)
					{
						if (my_raw_fileJ.used == false) return;
						if (my_raw_fileJ.biorep == my_brep && my_raw_fileJ.techrep == my_trep)
						{
							my_raw_fileJ.fraction = my_cur_fraction++;
						}
					});
			});	
		}
		else
		{//Label free cases:
			$.each(rawfiles_structure, function (idx, my_raw_file)
			{
				if (my_raw_file.used == false) return;
				// console.log("Checking fractionation in " + my_raw_file.rawfile);
				var my_brep = my_raw_file.biorep;
				var my_trep = my_raw_file.techrep;
				//In label free case the fractions are automatically set to ascending order
				//in groups that have not only biorep and techrep in common but freactions as well
				var my_assigned_condition = "";
				$.each(RawFileConditions, function (idxC, my_cond)
				{
					if (my_raw_file.rawfile == my_cond.name)
					{
						my_assigned_condition = my_cond.condition;
						return false;
					}
				});
				//if my_assigned_condition == "" then no condition has been assigned and this raw file can not be a part of any groups
				if (my_assigned_condition == "")
				{
					return; //continue
				}
				var my_cur_fraction = 1;
				$.each(rawfiles_structure, function (idxJ, my_raw_fileJ)
					{
						if (my_raw_fileJ.used == false) return;
						//Find out the assigned condition (if any) of the J raw file
						var my_assigned_conditionJ = "";
						$.each(RawFileConditions, function (idxJC, my_condJ)
						{
							if (my_raw_fileJ.rawfile == my_condJ.name)
							{
								my_assigned_conditionJ = my_condJ.condition;
								return false;
							}
						});
						//if my_assigned_conditionJ == "" then no condition has been assigned and this raw file can not be a part of any groups
						if (my_assigned_conditionJ == "")
						{
							return;
						}
						if (my_raw_fileJ.biorep == my_brep && my_raw_fileJ.techrep == my_trep && 			my_assigned_condition == my_assigned_conditionJ)
						{
							my_raw_fileJ.fraction = my_cur_fraction++;
						}
					});
			});
		}

		//Show the new structure to the user:
		var all_items = $('#rawfiles_tbl_allfiles').find('tr');
		// console.log(all_items);
		for (var i = 0; i < all_items.length; i++) {
         var items_tds = $(all_items[i]).find('td');
         var items_frac = items_tds[3];
		 // console.log("Reassigning fraction in " + $(items_tds[0]).text());
		 $.each(rawfiles_structure, function (idx, my_raw_file)
		 {
			 // console.log("Checking if " + my_raw_file.rawfile + " and " + $(items_tds[0]).text() + " are equal");
			 if (my_raw_file.rawfile == $(items_tds[0]).text())
			 {
				 // console.log("assigning fraction " + my_raw_file.fraction +  " in " + $(items_tds[0]).text());
				 $(items_frac).text(my_raw_file.fraction);
				 return false;
			 }
		 });
		}
}

var postTestDatasetInfo = function (dataset_desc) {
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   thedata.append('descriptions_requested', false);
   thedata.append('dataset_info_requested', dataset_desc);
   $.ajax({
      url: cgi_bin_path + 'load_test_data.php',
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false
   }).done(function (data, textStatus, jqXHR) {
//if there was a server-side error alert.
      if (!data.success) {
         alert("ERROR on SERVER: " + data.msg);
      } else {
         uploadingFiles = data.queryres.file;
         if (uploadingFiles && uploadingFiles.length > 0) {
//Start uploading ...
            uploadFiles(true, function () {
               if (++nUploaded < uploadingFiles.length) {
                  return;
               }
			   $("#s2btnb").prop('disabled', false);
			   $("#dlgTestDatasetsBtnOK").prop('disabled', false);
			   datatestOK_clicked = false;
               $("#s2btnf").triggerHandler("click");
               $("#s3showhideadvparams").trigger("click");
               $("input[name='expid']").val(dataset_desc.replace(/[^a-zA-Z0-9]+/g, "_"));
               $.each(data.queryres.selector, function (idx, param_selector)
               {
                  // console.log(param_selector + " = " + data.queryres.value[idx]);
                  switch ($(param_selector).attr('type')) {
                     case "checkbox":
                        // Here the 0/1 is transmitted false/true
                        var theval = (data.queryres.value[idx] == "0" ? false : true);
                        if ($(param_selector).prop("checked") != theval) {
                           $(param_selector).prop("checked", theval);
                        }
                        break;
                     default:
                        m = param_selector.match(/^#explbl[2-9]+[0-9]*name_$/);
                        if (m != null && !$(param_selector).is(':visible') && AddedLabels == false) {
						   // console.log(m);
						   if (AppendNewLabels == false)
						   {
							   $('#conditions_list').empty();
						   }
						   else
						   {
							   AppendNewLabels = false;
						   }
                           addFormLabel();
						   AddedLabels = true;
                        }
                        $(param_selector).val(data.queryres.value[idx]);
                        break;
                  }
               });
               for (var i = 0; i < data.queryres.raw_file.length; i++) {
                  rawfiles_structure.push({rawfile: data.queryres.raw_file[i], biorep: data.queryres.brep[i], techrep: data.queryres.trep[i], fraction: data.queryres.frac[i], used: data.queryres.used[i]});
				  if (data.queryres.condition[i] != "-")
				  {
					  RawFileConditions.push({name: data.queryres.raw_file[i], condition: data.queryres.condition[i]});
				  }
				  // console.log(RawFileConditions);
                  var tds = $('tr[id="tr_' + data.queryres.raw_file[i] + '"] td');
                  $(tds[1]).text(data.queryres.brep[i] == 0 ? '-' : data.queryres.brep[i]);
                  $(tds[2]).text(data.queryres.trep[i] == 0 ? '-' : data.queryres.trep[i]);
                  $(tds[3]).text(data.queryres.frac[i] == 0 ? '-' : data.queryres.frac[i]);
				  if (data.queryres.condition[i] != "-")
				  {
					$(tds[4]).text(data.queryres.condition[i]);
				  }
				  if (data.queryres.used[i] == 0)
				  {
					  $(tds[0])["0"].style = "text-decoration: line-through";
					  $(tds[1])["0"].style = "text-decoration: line-through";
					  $(tds[2])["0"].style = "text-decoration: line-through";
					  $(tds[3])["0"].style = "text-decoration: line-through";
					  if (isLabelFree == true) $(tds[4])["0"].style = "text-decoration: line-through";
				  }
               }
			   if (typeof data.queryres.option !== "undefined")
			   {
				for (var i = 0; i< data.queryres.option.length; i++)
				{
					switch (data.queryres.option[i])
					{
						//For the extra options:
						case "Rename":
							//The following automates the load of a test dataset that contains merged labels:
							if(!AllowMergeLabels) break;
							RenameArray = [];
							AuthenticItemsinRename = [];
							var my_val = data.queryres.opt_value[i];
							var lines = my_val.split("||");
							$.each(lines, function(idx, my_line){
								var my_values = my_line.split("|");
								RenameArray.push(my_values);
								AuthenticItemsinRename.push(my_values[0]);
							});
							RenameFromTestData = true;
							Refresh_conds_list();
							break;
						case "LS_Array":
							//The following automates the loading of a dataset that contains label swap, specifically it builds the LabelSwapArray
							if(!AllowLS) break;
							LabelSwapArray = [];
							var my_val = data.queryres.opt_value[i];
							var lines = my_val.split("||");
							$.each(lines, function(idx, my_line){
								var my_values = my_line.split("|");
								var to_add = [];
								to_add.push(my_values[0]);
								to_add.push(my_values[1]);
								to_add.push(my_values[2]);
								to_add.push(parseInt(my_values[3]));
								LabelSwapArray.push(to_add);
							});
							LS_array_counter = lines.length;
							break;
						case "LS_c_p_Add":
							//The following builds the LS_counters_per_Add array:
							if(!AllowLS) break;
							LS_counters_per_Add = [];
							var my_val = data.queryres.opt_value[i];
							var lines = my_val.split("||");
							$.each(lines, function(idx, my_line){
								var my_values = my_line.split("|");
								//my_values loads 5 values: the first one is the LS assignment's description, the second and third are the first and last indices of rows in LabelSwapArray that correspond to the specific assignment. The 4th and 5th are the two labels that are swapped
								var to_add = [];
								to_add[0] = my_values[0];
								to_add[1] = [];
								for(var i = parseInt(my_values[1]); i<= my_values[2]; i++)
								{
									to_add[1].push(i);
								}
								to_add[2] = [];
								to_add[2].push(my_values[3]);
								to_add[2].push(my_values[4]);
								LS_counters_per_Add.push(to_add);
								//Add the respective lines in LSLabelSwaps
								$("#LSLabelSwaps").append("<option value='" + my_values[0] + "'>" + my_values[0] + "</option>");
							});
							break;
						case "Select_Labels":
							//Selectes only the labels that are written in the database
							var my_val = data.queryres.opt_value[i];
							my_lbls_toselect = my_val.split("|");
							break;
					}
				}
			   }
               set_reps();
			   refresh_fractions();
			   if (isLabelFree == true) refresh_LFQ_conditions_from_test_data();
               $("#s3expparams").animate({scrollTop: 0}, "slow");
            });
         } else {
            $("#s2btnf").prop('disabled', true);
         }
      }
   }).fail(function (jqXHR, textStatus, errorThrown) {
      alert("An AJAX error occurred: " + errorThrown);
   });
}

var select_labels_according_to_test_dataset = function()
{
	if (my_lbls_toselect.length > 0 )
	{
		$("#conditions_list option").each(function(idx, my_opt)
		{
			if($.inArray($(my_opt).val(), my_lbls_toselect) != -1)
			{
				$(my_opt).prop("selected", true);
			}
			else
			{
				$(my_opt).prop("selected", false);
			}
		});
	}
}
var refresh_LFQ_conditions_from_test_data = function()
{
	RawFileConditions_copy = RawFileConditions.slice();
	$.each(rawfiles_structure, function (idx, my_raw_file)
	{
		if (my_raw_file.used == false)
		{//for each rawfile not used splice the respective row
			$.each(RawFileConditions_copy, function (idxC, my_cond)
			{
				if (my_raw_file.rawfile == my_cond.name)
				{//if there is a row in RawFileConditions_copy corresponding to a not used rawfile, erase this row
					RawFileConditions_copy.splice(idxC, 1);
					//normally there can not be two rows in RawFileConditions_copy with the same name so escape the loop if erased a row
					return false;
				}
			});
		}
	});
	var non_un_condiitions = RawFileConditions_copy.map(function(value, index){return value.condition;});
	var unique_conditions = ArrNoDupe(non_un_condiitions);
	$.each(unique_conditions, function (idx, my_un_cond)
	{
		$("#s2LFQConditionsList").append("<option val='undefined'>" + my_un_cond + "</option>");
	});
}
var postTestDatasetsInfo = function () {
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   thedata.append('descriptions_requested', true);
   $.ajax({
      url: cgi_bin_path + 'load_test_data.php',
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false
   }).done(function (data, textStatus, jqXHR) {
//if there was a server-side error alert.
      if (!data.success) {
         alert("ERROR on SERVER: " + data.msg);
      } else if(data.queryres.desc) {
         var i = 1;
         $.each(data.queryres.desc, function (idx, dataset_desc)
         {
            $("#s1TestDatasetsSelection").append("<option value='" + (i++) + "'>" + dataset_desc + "</option>");
         });
      }

   }).fail(function (jqXHR, textStatus, errorThrown) {
      alert("An AJAX error occurred: " + errorThrown);
   });
}

var bioreps;
var techreps;
var fractions;
var rawfiles;
var rawfiles_structure;
var rep_counts;
var lastclicked_rawfiles_tbl_tr = null;

//The following sub is for DEBUGGING purposes only, comment-out in deployment
var add_raw_file_structure = function(tab_sep_string)
{
	//This function prints all file names if tab_rep_string == "" or sets the rawfile_structure otherwise
	if(typeof(tab_sep_string) == "undefined" || tab_sep_string == "")
	{
		var all_items = $('#rawfiles_tbl_allfiles').find('tr');
		for (var i = 0; i < all_items.length; i++) {
			var items_tds = $(all_items[i]).find('td');
			console.log($(items_tds[0]).text() + "\n");
		}
		return;
	}
	rawfiles_structure = [];
	var lines = tab_sep_string.split("\t\t");
	$.each(lines, function(idx, my_line){
		var my_vals = my_line.split("\t");
		if(my_vals[0] == "rawfile")
		{
			return;
		}
		rawfiles_structure.push({rawfile: my_vals[0], biorep: my_vals[1], techrep: my_vals[2], fraction: my_vals[3], used: my_vals[4]});
	});
	
	//Show everything back to the user
		var all_items = $('#rawfiles_tbl_allfiles').find('tr');
		// console.log(all_items);
		for (var i = 0; i < all_items.length; i++) {
         var items_tds = $(all_items[i]).find('td');
         var items_brep = items_tds[1];
         var items_trep = items_tds[2];
         var items_frac = items_tds[3];
		 $.each(rawfiles_structure, function (idx, my_raw_file)
		 {
			 if (my_raw_file.rawfile == $(items_tds[0]).text())
			 {
				 $(items_brep).text(my_raw_file.biorep);
				 $(items_trep).text(my_raw_file.techrep);
				 $(items_frac).text(my_raw_file.fraction);
				 if(my_raw_file.used == false)
				 {
					 $(items_tds[0])["0"].style = "text-decoration: line-through";
				 }
				 return false;
			 }
		 });
		}
		refresh_fractions();
		set_s22btnf();
}

var reset_reps = function () {
   bioreps = 0;
   techreps = 0;
   fractions = 0;
   rawfiles_structure = [];
   RawFileConditions = [];
   rep_counts = {biorep: []};
   rawfiles_tbl_allfiles_DT.clear();
   $.each(rawfiles, function (idx, filename_i) {
      rawfiles_tbl_allfiles_DT.row.add(
              {
                 'fname': filename_i,
                 'brep': '-',
                 'trep': '-',
                 'frac': '-',
				 'cond': '-',
                 'DT_RowClass': "rawfiles_tbl_td_not_selected",
                 'DT_RowId': 'tr_' + filename_i
              }
      );
   });
   rawfiles_tbl_allfiles_DT.draw();
   $('#rawfiles_tbl_allfiles tbody tr').click(function (event) {
	   // console.log("			" + this);
      if (event.shiftKey) {
         if (lastclicked_rawfiles_tbl_tr !== null) {
            var i1 = $('#rawfiles_tbl_allfiles tbody tr').index(lastclicked_rawfiles_tbl_tr);
            var i2 = $('#rawfiles_tbl_allfiles tbody tr').index(this);
            var trs = $('#rawfiles_tbl_allfiles tbody tr');
            if (i2 > i1) {
               for (var i = (i1 + 1); i <= i2; i++) {
                  $(trs[i]).toggleClass('rawfiles_tbl_td_selected');
               }
            } else {
               for (var i = (i1 - 1); i >= i2; i--) {
                  $(trs[i]).toggleClass('rawfiles_tbl_td_selected');
               }
            }
         }
      } else {
         $(this).toggleClass('rawfiles_tbl_td_selected');
      }
      lastclicked_rawfiles_tbl_tr = this;
   });
}


function set_reps() {
   for (var i = 0; i < rawfiles_structure.length; i++) {
      var rep = "biorep";
      var max_level = 0;
      if (rawfiles_structure[i][rep] != 0) {
         if (!(rawfiles_structure[i][rep] in rep_counts[rep])) {
            rep_counts[rep][rawfiles_structure[i][rep]] = {techrep: []};
            bioreps++;
         }
      }
      rep = "techrep";
      if (rawfiles_structure[i][rep] != 0) {
         if (!(rawfiles_structure[i][rep] in rep_counts["biorep"][rawfiles_structure[i]["biorep"]][rep])) {
            rep_counts["biorep"][rawfiles_structure[i]["biorep"]][rep][rawfiles_structure[i][rep]] = {fraction: []};
            techreps++;
         }
         max_level = 1;
      }
      rep = "fraction";
      if (rawfiles_structure[i][rep] != 0) {
         if (!(rawfiles_structure[i][rep] in rep_counts["biorep"][rawfiles_structure[i]["biorep"]]["techrep"][rawfiles_structure[i]["techrep"]]["fraction"])) {
            rep_counts["biorep"][rawfiles_structure[i]["biorep"]]["techrep"][rawfiles_structure[i]["techrep"]]["fraction"][rawfiles_structure[i][rep]] = 1;
            fractions++;
         }
         max_level = 2;
      }
   }
//console.log(rep_counts);
//console.log([bioreps, techreps, fractions]);
	set_s22btnf();
   $("#btnResetExpStructCoord").prop('disabled', rawfiles_structure.length == 0);
   

}

function set_s22btnf()
{
	// console.log("rawfikes " + rawfiles.length + " rawfilesstructure: " + rawfiles_structure.length);
	if (rawfiles_structure.length == rawfiles.length) {	//if all files have been assigned to something
		var found_used_rec = false;
		$.each(rawfiles_structure, function (idx, my_raw_file)
		{
			// console.log(my_raw_file.rawfile);
			if(my_raw_file.used == true)
			{
				found_used_rec = true;
				return;
			}
		});
		var bioreps_used = [];
		$.each(rawfiles_structure, function (idx, my_raw_file)
		{
			if(my_raw_file.used == true)
			{
				if (bioreps_used.indexOf(my_raw_file.biorep) == -1)
				{
					bioreps_used.push(my_raw_file.biorep);
				}
			}
		});
		// console.log(bioreps_used);
		if(isLabelFree == false)
		{
			$("#s22btnf").prop('disabled', !(rawfiles.length > 0 && rawfiles_structure.length == rawfiles.length && found_used_rec && bioreps_used.length > 1));
		}
		else
		{
			//In case of label free data The next button should be enabled only if al used raw fileshave been assigned to a condition
			var all_files_have_label_assigned = true;
			$.each(rawfiles_structure, function (idx, my_raw_file)
			{

				if(my_raw_file.used == true)
				{
					//foreach used file in rawfiles structure see if there is a corresponding one in
					//Rawfileconditions
					var found_corresponding_file = false;
					$.each(RawFileConditions, function (idx, my_cond)
					{
						if (my_raw_file.rawfile == my_cond.name)
						{
							found_corresponding_file = true;
							return;
						}
					});
					if (found_corresponding_file == false)
					{
						all_files_have_label_assigned = false;
						return;
					}
				}
			});
			//Here if all used files have a corresponding label (condition) assigned all_files_have_label_assigned is set to true otherwise to false
			$("#s22btnf").prop('disabled', !(rawfiles.length > 0 && rawfiles_structure.length == rawfiles.length && all_files_have_label_assigned == true && found_used_rec && bioreps_used.length > 1));
		}

		
   }
   else
   {
	   $("#s22btnf").prop('disabled', true);
   }
}
var gen_expdesign = function (struct) {
   var ret = "";
   for (var i = 0; i < struct.length; i++) {
	   if(struct[i].used == true){
		ret = ret + struct[i].rawfile + "\t" + struct[i].biorep + "\t" + struct[i].techrep + "\t" + struct[i].fraction + "\n"; 
	   }

   }
   // console.log("ret: " + ret);
   return ret;
}

var gen_lfqdesign = function (struct) {
   var ret = "";
   for (var i = 0; i < struct.length; i++) {
		ret = ret + struct[i].name + "\t" + struct[i].condition + "\n"; 
   }
   // console.log("ret: " + ret);
   return ret;
}
var gen_LSArray = function (struct) {
   if(!AllowLS) return;
   var ret = "";
   for (var i = 0; i < struct.length; i++) {
		ret = ret + struct[i][0] + "\t" + struct[i][1] + "\t" + struct[i][2] + "\n"; 
   }
   if (ret == "")
   {
	   ret = "This run contains\tno label\tswaps\n"
   }
   console.log("ret: " + ret);
   return ret;
}

var gen_RenameFile = function (struct) {
   var ret = "";
   for (var i = 0; i < struct.length; i++) {
		ret = ret + struct[i][0] + "\t" + struct[i][1] + "\n"; 
   }
   // console.log("ret: " + ret);
   return ret;
}

var rss_i;
var renderRSSData = function (data, renderelem) {
   var prev_html = "<notset>";
   rss_i = 0;
   var items = Object.keys(data);
   items = shuffle(items);
   var updateFun = function () {
      // If the current html content of renderelem is not the same as the last one set by this function, it means some other function has set the html content so we have to terminate this infinite update of renderelem's content by not calling the setTimeout function (essentially calling oneself) and just returning.
      if (prev_html.localeCompare("<notset>") != 0 && $(renderelem).html().localeCompare(prev_html) != 0) {
         return;
      }
      if (rss_i == (items.length - 1)) {
         rss_i = 0;
      }
      var item = items[rss_i++];
      //console.log(data[item]);
      if (!/^http/.test(data[item]) || /gif|png|jpg|jpeg$/.test(data[item])) {
         inter = setTimeout(updateFun, 100);
         return;
      }
      $(renderelem).hide();
      prev_html = '<p>Inside the current issue of Nature Methods:<br><br><a href="' + data[item] + '" target="_blank"><strong>' + item + '</strong></a></p>';
      //$("#rsscontent").html('<p><a href="'+data[item]+'" target="_blank">'+item+'</a></p><br><iframe src="'+data[item]+'" style="display: block; width:100%; height:100%;"/>').fadeIn(300);
      $(renderelem).html(prev_html).fadeIn(300);
      var intertime = (Math.log((item.split(/\s/).length) + 1) / Math.log(1.6)) * 1000 + 1000;
      //console.log(item.split(/\s/).length + ': ' + intertime);
      inter = setTimeout(updateFun, intertime);
   }
   var inter = setTimeout(updateFun, 100);
}

var ons2LFQConditionsOK_click = function()
{
	//In case the user clicked on ok on the popup box trying to assign a condition to raw files update
	//assign the appropriate condition: in case the user added a new condition
	//update the list as well
	
	var CondToAdd;
	if ($("#s2LFQConditionsNew").prop("value") != "")
	{
		if (typeof($("#s2LFQConditionsNew").prop("value")) == "undefined") return;
		LFQconditions.push($("#s2LFQConditionsNew").prop("value"));
		CondToAdd = $("#s2LFQConditionsNew").prop("value");
		$("#s2LFQConditionsNew").val("");
		var ListLength = $("#s2LFQConditionsNew").prop("length");
		var optiontext = '<option val="' + ListLength + '">' + CondToAdd + '</option>';
		$("#s2LFQConditionsList").append(optiontext);
		$("#s2LFQConditionsList").attr("disabled", false);
	}
	else{
		if ($("#s2LFQConditionsList").prop("value") != "")
		{
			CondToAdd = $("#s2LFQConditionsList").prop("value");
		}
	}
	if (typeof(CondToAdd) == "undefined") return;
	//Now add the condition to the selected files:
	// console.log("Condition to add: " + CondToAdd);
	//First clean up all RawFileConditions rows that wil be overwritten
	var items = $('#rawfiles_tbl_allfiles').find('.rawfiles_tbl_td_selected');
	var item_idxs_to_splice = [];
	$.each(RawFileConditions, function (idx, my_cond)
	{
		
		for (var i = 0; i < items.length; i++) {
			var items_tds = $(items[i]).find('td');
			var items_name = items_tds[0];
			var name_txt = $(items_name).text();
			if(my_cond.name == name_txt)
			{
				item_idxs_to_splice.push(idx);
			}
		}
	});
	for (var i = item_idxs_to_splice.length -1; i >= 0; i--)
	{
		RawFileConditions.splice(item_idxs_to_splice[i], 1);
	}
	for (var i = 0; i < items.length; i++) {
		 var items_tds = $(items[i]).find('td');
		 var items_cond = items_tds[4];
		 var items_name = items_tds[0];
		//Now update the RawFileConditions list
		RawFileConditions.push({name: $(items_name).text(), condition: CondToAdd});
		$(items_cond).text(CondToAdd);
	}
	// console.log(RawFileConditions)
	//Deselect all rows
	var my_table = $('#rawfiles_tbl_allfiles').dataTable();
	var my_rows = my_table._('tr', {"filter":"applied"});
	$.each(my_rows, function (idx, cur_row) {
		// console.log(cur_row);
		var my_tmp = cur_row["DT_RowId"];
		$(document.getElementById(my_tmp.toString())).removeClass('rawfiles_tbl_td_selected');
	});
	//Also check if the next button should be enabled now
	set_s22btnf();
	//Also refresh the fractions:
	refresh_fractions();
}
var getRSS = function (rssurl, renderelem) {
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   thedata.append('rssurl', rssurl);
   $.ajax({
      url: cgi_bin_path + 'get_rss.php',
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false,
      beforeSend: function (jqXHR, settings) {
      }
   }).done(function (data, textStatus, jqXHR) {
      renderRSSData(data, renderelem);
   }).fail(function (jqXHR, textStatus, errorThrown) {
   });
}

var oncondslistclick = function(){
	   if (AllowMergeLabels)
	   {
		   Refresh_conds_list_cmenu_items();	
	   }
}

var dlgFadeout = function () {
   $(".expparamsDlg").fadeOut(300, function () {
      $('#mask').remove();
   });
}

var dlgFadeoutInfo = function () {
   $(".expparamsDlgInfo").fadeOut(300, function () {
      $('#maskInfo').remove();
   });
}

var label_context_menu;

$(document).ready(function () {
	// 	document.getElementById("s2LFQConditionsNew").onkeydown = inputChCheck(event,'^(?!_)[a-zA-Z0-9_]+$',20);
   if(!AllowLS)  $("#s3showdivLabelSwapRow").hide();
   var forward_buttons = getItems("button.main", /s[0-9]+btnf/);
   var backward_buttons = getItems("button.main", /s[0-9]+btnb/);
   // Binds the click event to "toggleNextClass" for each "forward button" (button with class button.main and id /s[0-9]+btnf/) 
   forward_buttons.forEach(function (btn) {
      $(btn).on("click", function () {
         toggleNextClass(idsToStrings(".main_div .main_section"), "hidden", true, executeStage);
         if ($(backward_buttons[0]).hasClass("hidden")) {
            $(backward_buttons[0]).removeClass("hidden");
         }
		rawfiles_tbl_allfiles_DT.columns.adjust().draw();
   });
   });
   // Binds the click event to "toggleNextClass" for each "backward button" (button with class button.main and id /s[0-9]+btnb/) 
   backward_buttons.forEach(function (btn) {
      $(btn).on("click", function () {
         toggleNextClass(idsToStrings(".main_div .main_section").reverse(), "hidden", true, rollbackStage);
		 rawfiles_tbl_allfiles_DT.columns.adjust().draw();
      });
   });
   // Bind event for file upload button
   $("#s2btnupld").on("click", function () {
      $("#__s2btnupld").click();
   });
   // Bind event when file(s) is/are chosen
   $("#__s2btnupld").change(function () {
		if ($("#__s2btnupld").val() == "") {
			return;
		}
      var fnames = "";
       //$("#s2uluploaders > table").empty();
	   //console.log("files: " + this.files.length)
      if (this.files.length > 0) {
//Start uploading ...
         uploadingFiles = this.files;
			if (window.File && window.FileReader && window.FileList && window.Blob) {
				oversized_files_idxs = [];
		      $.each(uploadingFiles, function (idx, file_i)
		      {
		      	if(file_i.size > 2147483648)
				// if(false)
		      	{
		      		oversized_files_idxs.push(idx);
		      	}
		      });				
			}
			if(oversized_files_idxs.length == 0)
			{
		      uploadFiles(false, function () {
				 // console.log("upload files report: nuploaded / uploaded_files / typeof rawfiles / peptideLabelsNamesFromFile.length / peptideLabelsFromFile.length : " + nUploaded + "/" + uploaded_files + "/" + typeof rawfiles + "/" + peptideLabelsNamesFromFile.length + "/" + peptideLabelsFromFile.length);
				 //Check what type of files has the user uploaded:
				 var Files_valid = false;
				 if (types_of_files_uploaded.indexOf("MQ") != -1 && types_of_files_uploaded.indexOf("MQP") != -1)
				 {
					 if(types_of_files_uploaded.indexOf("PD") == -1)
					 {
						 //Maxquant valid files:
						 Files_valid = true;
						 procProgram = "MQ";
						 $("#s3expparams input[name='exppddata']").prop('checked', false);
						 $("#quantsoftdetectedspan").text("MaxQuant");
						 $("#quantitation_prog_lbl").text("\u2014 Raw data were quantified with MaxQuant");
					 }
					 else
					 {
						procProgram = "";
						//Both MQ and PD files were uploaded:
						alert("WARNING: Some of the files you uploaded are processed by MaxQuant and some by Proteome Discoverer, we are sorry to inform you that it is impossible to proceed.");
						$("#s2btnf").prop('disabled', true);
						Files_valid = false;
					 }
				 }
				 if(types_of_files_uploaded.indexOf("PD") != -1)
				 {
					 if (types_of_files_uploaded.indexOf("MQ") == -1 && types_of_files_uploaded.indexOf("MQP") == -1)
					 {
						 //PD valid files:
						 Files_valid = true;
						 procProgram = "PD";
						 $("#s3expparams input[name='exppddata']").prop('checked', true);
						 $("#quantsoftdetectedspan").text("Proteome Discoverer");
						 $("#quantitation_prog_lbl").text("\u2014 Raw data were quantified with Proteome Discoverer\u2122");
					 }
					 //No need to check if both MQ and PD files were uploaded, this has already been done
				 }
				 //If the user has uploaded more than one file from the same type alert them:
				if ((procProgram == "MQ" && types_of_files_uploaded.filter(filterFunction).length > 2) 	|| (procProgram == "PD" && types_of_files_uploaded.filter(filterFunction).length > 1))
				{
					alert("WARNING: You uploaded too many files from " + procProgram + "! Please make sure you upload MultiConsensus files. Please refresh the current page and try again.");
					$("#s2btnf").prop('disabled', true);
					Files_valid = false;
				}
				 // console.log("Files valid" + Files_valid);
				 // console.log("	Check to enable next: nToUpload = " + nToUpload);
				 if (Files_valid == true && (upload_was_successful == true || (nToUpload == 0 && (peptideLabelsNamesFromFile.length > 0 || peptideLabelsFromFile.length > 0)))) {
					upload_was_successful = true;
		            $("#s2btnf").prop('disabled', false);
		         }
		      });			
			}
			else
			{
				alert("WARNING: At least one of the files chosen is oversized, we are sorry to inform you that it is impossible to proceed.");
	         $("#s2btnf").prop('disabled', true);
			}
      } else {
         //$("#s2btnf").prop('disabled', true);
      }
		$("#__s2btnupld").val("");
   });
   //Toggle visibility of table with advanced params
   $("#s3showhideadvparams").on("click", function () {
      var txt = $(this).text();
      if ($("#s3advparams").hasClass("hidden")) {
         $("#s3advparams").removeClass("hidden");
         $(this).text(txt.replace("Show", "Hide"));
         var parentdiv = $(this).closest("div");
         parentdiv.scrollTop(parentdiv.prop("scrollHeight"));
      } else {
         $("#s3advparams").addClass("hidden");
         $(this).text(txt.replace("Hide", "Show"));
      }
   });  
   $("#s3showdivLabelSwap").on("click", function () {
	    if(!AllowLS) return;
	   	$(".expparamsDlg").css({"left" : ($("body").width()/2) - ($("#divLabelSwap").width()/2)});
		$('body').append('<div id="mask"></div>');
		$("#divLabelSwap").fadeIn(300);
		$('#mask').fadeIn(300);
		$("#LSBack").on("click", function () {
			 dlgFadeout();
			 //ADD BACK RESULT HERE
		});
		InitializeLS();
   }); 
   //Add functionality of add/remove labels buttons
   $("#btnAddLabel").on("click", addFormLabel);
   $("#btnRemLabel").on("click", removeFormLabel);
   bind_explbldefinition_focus("#explbl" + nFormLabels + "definition");
   $("#dlglabelsBtnCancel").on("click", function () {
      $(".expparamsDlg").fadeOut(300, function () {
         $('#mask').remove();
      });
   });
   $("#dlglabelsBtnInvertSel").on("click", function () {
      var tmp = $("#s3expparamsDlgLabelsSelection option").not(".hidden").not(":selected");
      $("#s3expparamsDlgLabelsSelection option").not(".hidden").filter(":selected").prop('selected','');
      $(tmp).prop('selected','selected');
   });
   //Toggle visibility on inputs that are dependent to each other
   // #1: dependency between quantitation filtering settings
   $("#s3advparams input[name='expquantfilt']").on("click", function () {
      if ($(this).prop('checked')) {
         $("#s3advparams select[name='expquantfiltlbl']").closest("tr").removeClass("hidden");
         $("#s3advparams input[name='expquantfiltprot']").closest("tr").removeClass("hidden");
         var parentdiv = $(this).closest("div");
         parentdiv.scrollTop(parentdiv.prop("scrollHeight"));
      } else {
         $("#s3advparams select[name='expquantfiltlbl']").closest("tr").addClass("hidden");
         $("#s3advparams input[name='expquantfiltprot']").closest("tr").addClass("hidden");
      }
      ;
   });
   // #2: dependency between PD data and label definitions.
   $("#s3expparams input[name='exppddata']").on("click", function () {
   });
   //TODO
   // #3: dependency between background existence and ?
   $("#s3advparams input[name='explbl00']").on("click", function () {
      if ($(this).prop('checked')) {
         $("#s3advparams select[name='explbl0']").closest("tr").removeClass("hidden");
         if (nFormLabels > peptideLabelsNamesFromFile.length - 1) {
            removeFormLabel();
         }
         var parentdiv = $(this).closest("div");
         parentdiv.scrollTop(parentdiv.prop("scrollHeight"));
      } else {
         $("#s3expparams select[name='explbl0']").closest("tr").addClass("hidden");
      }
   });
   // TextField validation
   $("#s3expparams input[name='expbioreps']").on("focusout", function () {
      if ($(this).val() != "" && $("#s3expparams input[name='exptechreps']").val() != "" && (Number($(this).val()) + Number($("#s3expparams input[name='exptechreps']").val()) < 3)) {
         $(this).focus().select();
      }
   });
   $("#s3expparams input[name='exptechreps']").on("focusout", function () {
      if ($(this).val() != "" && $("#s3expparams input[name='expbioreps']").val() != "" && (Number($(this).val()) + Number($("#s3expparams input[name='expbioreps']").val()) < 3)) {
         $(this).focus().select();
      }
   });
   // Needed after form validation (because some required fields might have been highlighted)
   $("#s3expparams input[data-required]").on("focusout", function () {
      resetCssStyles(this);
   });
   // Display chopped long raw file name on a tooltip
   $('#rawfiles_tbl_allfiles').delegate('td', 'mouseenter', function () {
      var $this = $(this);
      if (this.offsetWidth < this.scrollWidth && !$this.attr('title')) {
         $this.attr('title', $this.text());
      }
   });

   $('#btnAssignExpStructCoord').on("click", function () {
      var items = $('#rawfiles_tbl_allfiles').find('.rawfiles_tbl_td_selected');
      var def_biorep = Number($('#expstructcoord_biorep').val());
      var def_techrep = Number($('#expstructcoord_techrep').val());
      var curr_biorep = def_biorep;
      var curr_techrep = def_techrep;
      var curr_fraction = 0;
      var rep_offset = 1;
      // if (def_biorep > 0 && def_biorep > (bioreps + 1)) {	// return if user tries to assign biorep X without having defined biorep X-1
         // return;
      // }
      // if (def_techrep > 0 && def_techrep > (techreps + 1)) {	// same as above but for techreps
         // return;
      // }
      $("#btnResetExpStructCoord").prop('disabled', false);
      for (var i = 0; i < items.length; i++) {
         var items_tds = $(items[i]).find('td');
         var items_biorep = items_tds[1];
         var items_techrep = items_tds[2];
         var items_frac = items_tds[3];
		 if (def_biorep == 0)
		 {
			 return;
		 }

				 //if the user attempted to overwrite the coordinations of a file, delete its rawfiles_structure entry and set it again
				$.each(rawfiles_structure, function (idx, my_raw_file)
				{
					// console.log("\t\t" + my_raw_file.rawfile + "\t" + $(items_tds[0]).text());
					if(my_raw_file.rawfile == $(items_tds[0]).text())
					{
						// console.log("\t\t" + my_raw_file.rawfile + " was attempted to be overwritten!");
						rawfiles_structure.splice(idx, 1);
						// console.log(rawfiles_structure);
						return false;
					}
				});
            if (def_biorep > 0) { // i.e. non-blank
               if (def_techrep == 0) { // blank
//techreps, from current count to items.length, except we have label-free data (peptideLabelsNamesFromFile.length == 0)
                  if (def_biorep in rep_counts["biorep"] && peptideLabelsNamesFromFile.length > 0) {
                     curr_techrep = (rep_counts["biorep"][def_biorep].techrep.length - 1) + rep_offset++;
                  } else {
                     curr_techrep = rep_offset++;
                  }
               } else {
// fractions, from current count to items.length, except we have label-free data (peptideLabelsNamesFromFile.length == 0)
                  if (def_biorep in rep_counts["biorep"] && def_techrep in rep_counts["biorep"][def_biorep].techrep && peptideLabelsNamesFromFile.length > 0) {
                     curr_fraction = (rep_counts["biorep"][def_biorep].techrep[def_techrep].fraction.length - 1) + rep_offset++;
                  } else {
                     curr_fraction = rep_offset++;
                  }
               }
            } else {
               if (def_techrep == 0) { // blank
// bioreps, from current count to items.length, except we have label-free data (peptideLabelsNamesFromFile.length == 0)
                  if (peptideLabelsNamesFromFile.length > 0) {
                     curr_biorep = rep_counts["biorep"].length + rep_offset++;
                  } else {
                     curr_biorep = rep_offset++;
                  }
               } else {
// error, a biorep must be specified
                  if (rawfiles_structure.length == 0) {
                     $("#btnResetExpStructCoord").prop('disabled', true);
                  }
                  return;
               }
            }
			curr_used = true;
            rawfiles_structure.push({rawfile: $(items_tds[0]).text(), biorep: curr_biorep, techrep: curr_techrep, fraction: curr_fraction, used : curr_used});
			$(items_tds[0])[0].style = "text-decoration: none";
			$(items_tds[1])[0].style = "text-decoration: none";
			$(items_tds[2])[0].style = "text-decoration: none";
			$(items_tds[3])[0].style = "text-decoration: none";
			if (isLabelFree == true) $(items_tds[4])[0].style = "text-decoration: none";
            $(items_biorep).text(curr_biorep == 0 ? '-' : curr_biorep);
            $(items_techrep).text(curr_techrep == 0 ? '-' : curr_techrep);
            $(items_frac).text(curr_fraction == 0 ? '-' : curr_fraction);
      }
      set_reps();
	  refresh_fractions();
	  $('#rawfiles_tbl_allfiles tbody tr').removeClass('rawfiles_tbl_td_selected');
   });
   $('#btnResetExpStructCoord').on("click", function () {
      reset_reps();
	  $("#btnResetExpStructCoord").prop('disabled', true);
      $("#s22btnf").prop('disabled', true);
   });
   // CSS
   $(".tooltip").hover(function () {
      $(".tooltip span").css({"margin-left": -$(".tooltip span").width() / 2 + 9});
      $(".callout").css({"left": $(".tooltip span").width() / 2});
   });
   $(".tooltip2").hover(function () {
      $(".tooltip2 span").css({"margin-left": 11});
      $(".callout2").css({"left": 0});
   });
   // Bind test datasets dialog buttons
   $("#dlgTestDatasetsBtnOK").on("click", function () {
	  dlgFadeout();
	  postTestDatasetInfo($("#s1TestDatasetsSelection option:selected").text());
	  $("#s2btnb").prop('disabled', true);
	  $("#dlgTestDatasetsBtnOK").prop('disabled', true);
	  datatestOK_clicked = true;
   });
   $("#dlgTestDatasetsBtnCancel").on("click", function () {
      dlgFadeout();
   });
   $("#dlgTestDatasetsBtnDownload").on("click", function () {
      dlgFadeout();
      var tdname = $("#s1TestDatasetsSelection option:selected").text();
      if(tdname){
         downloadTestDataset(tdname);
      }
   });
   // Initialize Raw file DataTable
   rawfiles_tbl_allfiles_DT = $('#rawfiles_tbl_allfiles').DataTable({
      paging: false,
      bInfo: false,
	  select: true,
	  "pageLength": 308,
	  scrollY: "142px",
	  scrollCollapse: true,
	  
      "columnDefs": [
         {
            targets: [1, 2, 3],
            width: '4%',
            className: "dt-center"
         },
         {
            targets: 0,
            width: '70%',
            className: "dt-left",
			title: "Raw File"
         },
		 {
			 targets: 1,
			title: "#B"
		 },
		 {
			 targets: 2,
			title: "#T"
		 },
		 {
			 targets: 3,
			title: "#F"
		 },
		 {
			 targets: 4,
			 title: "Condition",
			 width: '18%',
			 visible: false,
			 className: "dt-center"
		 }
      ],
      "dom": '<"top ibvspace"f>rtT',
		"sDom": 'lfrtip',
		"oLanguage": {
         "sSearch": "Filter: "
      },
      "aoColumns": [
         {"mData": "fname"},
         {"mData": "brep"},
         {"mData": "trep"},
         {"mData": "frac"},
		 {"mData": "cond"}
      ]
   });
   // Initialize the context menu 
     main_context_menu = $(document).contextmenu({
		delegate: ".dataTable td",
		menu: [
		  {title: "Select All", cmd: "slc_all"},
		  {title: "Deselect All", cmd: "dslc_all"},
		  {title: "Invert Selection", cmd: "inv_slc"},
		  {title: "Exclude Selected", cmd: "rmv_slc"},
		  {title: "Include Selected", cmd: "add_slc"},
		  {title: "Clear filter", cmd: "clr_fltr"},
		  {title: "Reset", cmd: "reset"},
		  {title: "Select unassigned", cmd: "slc_unassigned"},
		  {title: "Assign Condition", cmd: "assign_condition", visible: false}
		],
		select: function(event, ui) {
			switch(ui.cmd){
				case "slc_all":
					var my_table = $('#rawfiles_tbl_allfiles').dataTable();
					var my_rows = my_table._('tr', {"filter":"applied"});
					$.each(my_rows, function (idx, cur_row) {
						// console.log(cur_row);
						var my_tmp = cur_row["DT_RowId"];
						$(document.getElementById(my_tmp.toString())).addClass('rawfiles_tbl_td_selected');
						});
					break;
				case "dslc_all":
					var my_table = $('#rawfiles_tbl_allfiles').dataTable();
					var my_rows = my_table._('tr', {"filter":"applied"});
					$.each(my_rows, function (idx, cur_row) {
						// console.log(cur_row);
						var my_tmp = cur_row["DT_RowId"];
						$(document.getElementById(my_tmp.toString())).removeClass('rawfiles_tbl_td_selected');
						});
					break;
				case "inv_slc":
					var my_table = $('#rawfiles_tbl_allfiles').dataTable();
					var my_rows = my_table._('tr', {"filter":"applied"});
					$.each(my_rows, function (idx, cur_row) {
						// console.log(cur_row);
						var my_tmp = cur_row["DT_RowId"];
						$(document.getElementById(my_tmp.toString())).toggleClass('rawfiles_tbl_td_selected');
						});	
					break;
				case "clr_fltr":
					// var my_D_table = $('#rawfiles_tbl_allfiles').DataTable();
					rawfiles_tbl_allfiles_DT.search('');
					rawfiles_tbl_allfiles_DT.columns().search('');
					rawfiles_tbl_allfiles_DT.draw();
					break;
				case "reset":
					$("#btnResetExpStructCoord").trigger("click");
					break;
				case "rmv_slc":
					var my_table = $('#rawfiles_tbl_allfiles').dataTable();
					var items = $('#rawfiles_tbl_allfiles').find('.rawfiles_tbl_td_selected');
					for (var i = 0; i < items.length; i++) {
						var items_tds = $(items[i]).find('td');
						// console.log("rmvslc: " + $(items_tds[0])["0"].textContent + "items length:" +  items.length);
						// console.log($(items_tds[0]));
						var rec_found = false;
						$.each(rawfiles_structure, function (idx, my_raw_file)
						{
							// console.log(my_raw_file.rawfile);
							if(my_raw_file.rawfile == $(items_tds[0])["0"].textContent)
							{

								my_raw_file.used = false;
								rec_found = true;
								return false;
								// console.log("continued!");
							}
						});
						if (!rec_found)
						{
							rawfiles_structure.push({rawfile: $(items_tds[0]).text(), biorep: "-", techrep: "-", fraction: "-", used : false});
						}
						// console.log("stopped!");
						// console.log("rawfiles_structure after delete");
						// console.log(rawfiles_structure);
						$(items_tds[0])["0"].style = "text-decoration: line-through";
						$(items_tds[1])["0"].style = "text-decoration: line-through";
						$(items_tds[2])["0"].style = "text-decoration: line-through";
						$(items_tds[3])["0"].style = "text-decoration: line-through";
						if (isLabelFree == true) $(items_tds[4])["0"].style = "text-decoration: line-through";
					}
					set_s22btnf();
					$('#rawfiles_tbl_allfiles tbody tr').removeClass('rawfiles_tbl_td_selected');
					$("#btnResetExpStructCoord").prop('disabled', rawfiles_structure.length == 0);
					refresh_fractions();
					break;
				case "add_slc":
					var my_table = $('#rawfiles_tbl_allfiles').dataTable();
					var items = $('#rawfiles_tbl_allfiles').find('.rawfiles_tbl_td_selected');
					for (var i = 0; i < items.length; i++) {
						var items_tds = $(items[i]).find('td');
						// console.log(i);
						// console.log("addslc: " + $(items_tds[0])["0"].textContent + "items length:" +  items.length);
						var rec_found = false;
						$.each(rawfiles_structure, function (idx, my_raw_file)
						{
							// console.log(my_raw_file.rawfile);
							if(my_raw_file.rawfile == $(items_tds[0])["0"].textContent)
							{
								if(!(my_raw_file.biorep == '-' && my_raw_file.techrep == '-' && my_raw_file.fraction == '-'))
								{
									// console.log(my_raw_file.biorep);
									my_raw_file.used = true;
									rec_found = true;
								}
								else
								{
									rawfiles_structure.splice(idx, 1);
								}
								return false;
								// console.log("continued!");
							}
						});
						// console.log("stopped!");
						// console.log("rawfiles_structure after delete");
						// console.log(rawfiles_structure);
						$(items_tds[0])["0"].style = "text-decoration: none";
						$(items_tds[1])["0"].style = "text-decoration: none";
						$(items_tds[2])["0"].style = "text-decoration: none";
						$(items_tds[3])["0"].style = "text-decoration: none";
						if (isLabelFree == true) $(items_tds[4])["0"].style = "text-decoration: none";
					}
					set_s22btnf();
					$('#rawfiles_tbl_allfiles tbody tr').removeClass('rawfiles_tbl_td_selected');
					$("#btnResetExpStructCoord").prop('disabled', rawfiles_structure.length == 0);
					refresh_fractions();
					break;
				case "slc_unassigned":
					var my_table = $('#rawfiles_tbl_allfiles').dataTable();
					var my_rows = my_table._('tr', {"filter":"applied"});
					$.each(my_rows, function (idx, cur_row) {
						// console.log(cur_row);
						var my_tmp = cur_row["DT_RowId"];
						var rec_found = false;
						var unused_file = false;
						$.each(rawfiles_structure, function (idx, my_raw_file)
						{
							// console.log(my_raw_file.rawfile == cur_row["fname"]);
							if(my_raw_file.rawfile == cur_row["fname"])
							{
								rec_found = true;
							}
						});
						if (isLabelFree == false)
						{// in labelled data a row is unassigned if it is not found in the rawfiles_structure array
							if(rec_found == false)
							{
								$(document.getElementById(my_tmp.toString())).addClass('rawfiles_tbl_td_selected');
							}
							else
							{
								$(document.getElementById(my_tmp.toString())).removeClass('rawfiles_tbl_td_selected');
							}
						}
						else
						{// in label free the row is also unassigned if a condition is not assigned to it
							var cond_found = false;
							$.each(RawFileConditions, function (idxC, my_cond)
							{
								if (cur_row["fname"] == my_cond.name)
								{
									cond_found = true;
								}
							});
							if(rec_found == true && cond_found == true)
							{
								$(document.getElementById(my_tmp.toString())).removeClass('rawfiles_tbl_td_selected');
							}
							else
							{
								$(document.getElementById(my_tmp.toString())).addClass('rawfiles_tbl_td_selected');
							}							
						}


					});
					break;
				case "assign_condition":
					onAssignCondition();
					break;
				}
			},
		beforeOpen: function(event, ui) {
			var $menu = ui.menu,
				$target = ui.target,
				extraData = ui.extraData;
			ui.menu.zIndex(9999);
		}
	  });
		if(AllowMergeLabels){
		 label_context_menu = $('#conds_list_container').contextmenu({
			 delegate: ".conditions_list",
			 menu: [
				{title: "Same Condition", cmd: "same_cond"},
				{title: "Restore Conditions", cmd: "restore_conds"}
			 ],
			 select: function(event, ui) {
				 switch(ui.cmd){
					 case "same_cond":
						itemsToRename = [];
						$("#conditions_list option:selected").each(function(){ itemsToRename.push($(this).val());});
						// console.log(my_items);
						$(".expparamsDlg").css({"left" : ($("body").width()/2) - ($("#condsadvancedDlg").width()/2)});
						$('body').append('<div id="mask"></div>');
						$("#condsadvancedDlg").fadeIn(300);
						$('#mask').fadeIn(300);
						$("#s3AdvancedOK").on("click", function () {
							 dlgFadeout();
							 if($("#s3AdvNewCondition").val() == "") return;
							 $.each(RenameArray, function(idx, my_row)
							 {
								 if($.inArray(my_row[0], itemsToRename) != -1)
								 {
									 RenameArray[idx][1] = $("#s3AdvNewCondition").val();
								 }
							 });
							 // console.log(RenameArray);
							 $("#s3AdvNewCondition").val("");
							 RenameFromTestData = false;
							 Refresh_conds_list();
							 //ADD OK RESULT HERE
						});
						$("#s3AdvancedCancel").on("click", function () {
							 dlgFadeout();
							 //ADD cancel RESULT HERE
						});
						break;
					 case "restore_conds":
						itemsToRename = [];
						$("#conditions_list option:selected").each(function(){ itemsToRename.push($(this).val());});
						$.each(RenameArray, function(idx, my_row)
						 {
							 if($.inArray(my_row[1], itemsToRename) != -1)
							 {
								 RenameArray[idx][1] = RenameArray[idx][0];
							 }
						 });
						 RenameFromTestData = false;
						 Refresh_conds_list();
						break;
				 }
			 },
			 beforeOpen: function(event, ui) {
				var $menu = ui.menu,
					$target = ui.target,
					extraData = ui.extraData;
				ui.menu.zIndex(9999);
			}
		 });
		}
   $('#rawfiles_tbl_allfiles').css({border: 'none'});
   $('#rawfiles_tbl_allfiles').css({margin: '0px'});
   $('#rawfiles_tbl_allfiles thead th').css({border: 'none'});
   $('#rawfiles_tbl_allfiles').css({'border-bottom': 'none'});
   $('#rawfiles_tbl_allfiles').css({'border-bottom': 'none'});
   $('#btnRawfilesTblSelectAll').on("click", function () {
      $('#rawfiles_tbl_allfiles tbody tr').addClass('rawfiles_tbl_td_selected');
   });
   $('#btnRawfilesTblSelectNone').on("click", function () {
      $('#rawfiles_tbl_allfiles tbody tr').removeClass('rawfiles_tbl_td_selected');
   });
   $('#btnRawfilesTblDelete').on("click", function () {
      if ($('#rawfiles_tbl_allfiles .rawfiles_tbl_td_selected').length == 0) {
         return;
      }
      rawfiles_to_rem = $.map($('#rawfiles_tbl_allfiles .rawfiles_tbl_td_selected'), function (val, i) {
         return $(val).prop('id');
      });
      rawfiles = $.grep(rawfiles, function (n, i) {
         return rawfiles_to_rem.indexOf('tr_' + n) == -1;
      });
      $("#s3expparamsDlgLabelsSelection .hidden").removeClass('hidden');
      $("#s3expparamsDlgLabelsSelection option").each(function (i) {
         if (rawfiles.indexOf($(this).val()) == -1) {
            $(this).addClass('hidden');
         }
      });
      if ($('#rawfiles_tbl_allfiles .rawfiles_tbl_td_selected td:nth-child(2)').text().match(/[^-]+/g) != null) {
         rawfiles_tbl_allfiles_DT.rows($('#rawfiles_tbl_allfiles .rawfiles_tbl_td_selected')).remove();
         reset_reps();
      } else {
         rawfiles_structure = $.grep(rawfiles_structure, function (n, i) {
            return rawfiles_to_rem.indexOf('tr_' + n.rawfile) == -1;
         });
         rawfiles_tbl_allfiles_DT.rows($('#rawfiles_tbl_allfiles .rawfiles_tbl_td_selected')).remove();
         rawfiles_tbl_allfiles_DT.draw();
      }
      $("#s22btnf").prop('disabled', !(rawfiles.length > 0 && rawfiles_structure.length == rawfiles.length));
      $("#btnResetExpStructCoord").prop('disabled', rawfiles_structure.length == 0);
   });
	//from http://stackoverflow.com/questions/8641729/how-to-avoid-the-need-for-ctrl-click-in-a-multi-select-box-using-javascript
	$("#conditions_list").mousedown(function(e){
		e.preventDefault();
		if (event.which == 3)
		{
			return;
		}
		var select = this;
		var scroll = select .scrollTop;

		e.target.selected = !e.target.selected;

		setTimeout(function(){select.scrollTop = scroll;}, 0);

		$(select ).focus();
	}).mousemove(function(e){e.preventDefault()});
	
   // TEST DATA INIT
   postTestDatasetsInfo();
   //
   postClientServerClientInfo();
});