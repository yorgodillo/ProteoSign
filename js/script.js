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
//Advanced Parameters:
var LeastBRep = 2;// least breps where a protein must be found in order not to be disqualified
var LeastPep = 2; //least peptides where a protein must be found in order not to be disqualified
var Pthreshold = 0.05; //max p where a protein is considered as differentially expressed
var my_step = 0;
//The following var is used to ensure that the feedback button will be animated just once
var feedbackAnimated = false;
var carefulBackStep = 2;//in two cases (step 2 and 4) the back button should not immediatelly move the procedure one step back but shold ask the user first, this variable takes care of this situation
var RreturnedErrorsInCurSession = false;
//the following vars are for star-based feedback, star_hovered is the last star that tne mouse of the user went over and star_chosen is how many stars the user really chosen
var star_hovered = 1;
var star_chosen = 1;
var stars_enabled = true;
//the following line is for logging purposes
var log_test_dataset = false;
//The following vars are used for replication multiplexing
var RM_RMstep = 1;
var RMrawfilesdata = []; //RMrawfilesdata contains all the information for the rawfiles in case Replication Multiplexing is chosen, RMrawfilesdata is an array of 2 dimensions, each raw is a record that contains: an id, the name of the raw file, the brep the trep the frac and the condition assigned, if it is used or not and if it is currently selected or not
var RMtagsdata = []; // RMtagsdata contains all the information for the tags in case Replication Multiplexing is chosen, in the same way as RMrawfilesdata does for the raw files, notice that the fractions column is unnecessary and kept only for compatibility and simplicity reasons...
var RMbrepsRepInRawFiles = true;
var RMtrepsRepInRawFiles = true;
var RMconditionsRepInRawFiles = true;
var RMtags = []; //the tags of the experiment
var RMisused = false; //this will tell R if Replication Multiplexing will be used
var stepRM2initialized = false;
var stepRM3initialized = false;

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
var RMrawfilesDT;
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
// from http://stackoverflow.com/questions/1303646/check-whether-variable-is-number-or-string-in-javascript
function StringisNumber(n) { return /^-?[\d.]+(?:e-?\d+)?$/.test(n); } 

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

var InitializeAdvParam = function()
{
	$("#AdvParamLPep").empty();
	for(var i = 1; i < 11; i++)
	{
		if(i == LeastPep)
		{
			$("#AdvParamLPep").append("<option value='" + i + "' selected='true'>" + i + "</option>");
		}
		else{
			$("#AdvParamLPep").append("<option value='" + i + "'>" + i + "</option>");
		}
	}
	$("#AdvParamLBRep").empty();
	var my_bioreps = [];
	$.each(rawfiles_structure, function (idx, my_raw_file)
	 {
		 if (my_raw_file.used == false) return;
		 my_bioreps.push(my_raw_file.biorep);
	 });
	 var unique_breps = ArrNoDupe(my_bioreps);
	for(var i = 1; i < unique_breps.length + 1; i++)
	{
		if(i == LeastBRep)
		{
			$("#AdvParamLBRep").append("<option value='" + i + "' selected='true'>" + i + "</option>");
		}
		else{
			$("#AdvParamLBRep").append("<option value='" + i + "'>" + i + "</option>");
		}	
	}
	$("#PThreshold").val(Pthreshold);
}

var onADVoptionsOK_click = function()
{
	$("#PThreshold").val($("#PThreshold").val().replace(",", "."));
	if (StringisNumber($("#PThreshold").val()))
	{
		if ($("#PThreshold").val() > 0 && $("#PThreshold").val() < 1)
		{
			Pthreshold = $("#PThreshold").val();
		}
		else
		{
			setItemAttrColor("#PThreshold", "border", "#E60000");
			return;	
		}
	}
	else{
		setItemAttrColor("#PThreshold", "border", "#E60000");
		return;
	}
	LeastPep = $("#AdvParamLPep").val();
	LeastBRep = $("#AdvParamLBRep").val();
	setItemAttrColor("#PThreshold", "border", "#DDDDDD");
	dlgFadeout();
}

var onADVoptionsCancel_click = function()
{
	setItemAttrColor("#PThreshold", "border", "#DDDDDD");
	dlgFadeout();
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
	 // console.log(LabelSwapArray);	
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
	for(var i = LS_counters_per_Add.length - 1; i >= 0;i--)
	{
		if($.inArray(LS_counters_per_Add[i][0], options_to_erase) != -1)
		{
			LS_counters_per_Add.splice(i, 1);
		}
	}
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
			// console.log($(my_option).val());
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
var ons4btnfclick = function()
{
	if (feedbackAnimated == false)
	{
		$("#feedback_div").animate({
			width: "30px",
			opacity: 1
		}, 1000, function(){});
		feedbackAnimated = true;
	}
}
var onRMDialogCancel_click = function ()
{
	if (RMisused && is_RM_ready() == false)
	{
		questionbox("The data provided for <i>Replication Multiplexing</i> are not enough. Do you want to discard <i>Replication Multiplexing</i>?", function()
		{
			//the postyes
			set_RMisused(false, false);
			dlgFadeoutRepMult();
		},
		function()
		{
			//postNo
			
		});
	}
	else
	{
		dlgFadeoutRepMult();
	}
}
var onFeedbackclick = function()
{
	$(".expparamsDlgFeedback").css({"left": ($("body").width() / 2) - ($("#s4FeedbackPanel").width() / 2)});
	$('body').append('<div id="maskFeedback"></div>');
	$("#s4FeedbackPanel").fadeIn(300);
	$('#maskFeedback').fadeIn(300);
	$("#FeedbackPanelSend").unbind();
	$("#FeedbackPanelSend").on("click", function () {
		
		//ADD SEND RESULT HERE
	});
	$("#FeedbackPanelCancel").unbind();
	$("#FeedbackPanelCancel").on("click", function () {
		dlgFadeoutFeedback();
		$("#userFeedback").val("");
		var mytext = 600 + " characters left";
		$("#FBcharleft").text(mytext);
		//ADD CANCEL RESULT HERE
	});
}

var onFeedbackPanelSendclick = function()
{
	if (parseInt($("#userFeedback").val().length) == 0 && !stars_enabled)
	{
		msgbox("<p>Please fill the text box with your feedback.</p>");
		return;
	}
	var thedata = new FormData();
	thedata.append('texttoappend', $("#userFeedback").val());
	thedata.append('session_id', sessionid);
	if (!stars_enabled)
	{
		thedata.append('stars_score', "NA");
	}
	else
	{
		thedata.append('stars_score', star_chosen);
	}
	$.ajax({
	  url: cgi_bin_path + 'send_feedback.php', //Server script to send the feedback
	  type: 'POST',
	  // Form data
	  data: thedata,
	  //Options to tell jQuery not to worry about content-type.
	  processData: false,
	  cache: false,
	  contentType: false,
   }).done(function (data, textStatus, jqXHR) {
	   if (data.success == true)
	   {
			msgbox("<p>Thank you! Your feedback has been submitted successfully!</p>");
			dlgFadeoutFeedback();
			$("#userFeedback").val("");
			var mytext = 600 + " characters left";
			$("#FBcharleft").text(mytext);
	   }
	   else{
		   msgbox("<p>An error occured! Please try again.</p>");
	   }
   }).fail(function (jqXHR, textStatus, errorThrown){
	   msgbox("<p>An error occured! Please try again.</p>");
   });
}
var ons4btnbclick = function()
{
	$("#server_feedback").css("box-shadow", "")
	
}
var onuserFeedbackchange = function()
{
	var mynum = 600 - parseInt($("#userFeedback").val().length);
	var mytext = mynum + " characters left";
	$("#FBcharleft").text(mytext);
}
var inputChCheck = function (e, repatt, maxCharacters) {
   var theEvent = e || window.event;
   var key = theEvent.keyCode || theEvent.which;
   var re = new RegExp(repatt);
   var srcelem = e.target || e.srcElement;
   var carposition = srcelem.selectionStart; // the carret position
   if (srcelem.selectionEnd - srcelem.selectionStart >= 1)
   {
	   maxCharacters = 9999;
   }
   var txt = [srcelem.value.slice(0, carposition), String.fromCharCode(e.which), srcelem.value.slice(carposition)].join('');
   if (key === 8) {
      return;
   }

   if (!re.test(txt) || txt.length > maxCharacters) {
      e.preventDefault();
   }

}

var inputPasteCheck = function(e, repatt, maxCharacters)
{
	//this function makes sure that in case the user pastes data in a text box where character rules apply, the data will contain only valid characters
	var re = new RegExp(repatt);
	var srcelem = e.target || e.srcElement;
	var carposition = srcelem.selectionStart; // the carret position
	var clipboardData, pastedData;
	e.preventDefault();
	clipboardData = e.clipboardData || window.clipboardData;
	pastedData = clipboardData.getData('Text'); //the data that the user tries to paste
	if (pastedData == "") return;
	//make sure that the data contain only valid characters, in case of invalid chars transform them to underscores
	if (!re.test(pastedData))
	{
		pastedData = pastedData.replace(/[^a-zA-Z0-9]+/g, "_");
		
	}
	if(srcelem.selectionStart == 0)
	{
		pastedData = pastedData.replace(/^[0-9_]*/g, "");
	}
	var srcValue = srcelem.value;
	if (srcelem.selectionEnd - srcelem.selectionStart >= 1)
	{
		srcValue = [srcValue.slice(0, srcelem.selectionStart), srcValue.slice(srcelem.selectionEnd)].join('');
	}
	//the text that will be produced will be:
	var txt = [srcValue.slice(0, carposition), pastedData, srcValue.slice(carposition)].join('');
	if(txt.length > maxCharacters)
	{
		txt = txt.slice(0, maxCharacters)
	}
	srcelem.value = txt;
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

var questionbox = function(caption, postYes, postNo)
{
	
	$(".expparamsDlgInfo").css({"left": ($("body").width() / 2) - ($("#VariousInfoDialog").width() / 2)});
	$('body').append('<div id="maskInfo"></div>');
	$("#QuestionBoxDialog").fadeIn(300);
	$('#maskInfo').fadeIn(300);
	$("#CaptionDiv").empty();
	$("#CaptionDiv").append("<span>" + caption + "</span>");
	//in a question box, some functions should be executed after clicking on yes and no, these are sent as parameters to question box:
	//to assign them first remove any handlers from QBYes and QBNo and reassign the sent functions
	//also make sure that the dialog closes after hitting yes or no$("#QBYes").off("click");
	$("#QBYes").off("click");
	$("#QBYes").on("click", postYes);
	$("#QBYes").on("click", function(){
		dlgFadeoutInfo();
	});
	
	$("#QBNo").off("click");
	$("#QBNo").on("click", postNo);
	$("#QBNo").on("click", function() {
		dlgFadeoutInfo();
	});
}

var showRepMultDialog = function()
{
	RM_RMstep = 1;
	showstepRMDialog();
	$(".RepMultDlg").css({"left": ($("body").width() / 2) - ($("#ReplicationMultiplexingDialog").width() / 2)});
	$(".RepMultDlg").css({"top": ($("body").height() / 2) - ($("#ReplicationMultiplexingDialog").height() / 2)});
	$('body').append('<div id="maskRepMult"></div>');
	$("#ReplicationMultiplexingDialog").fadeIn(300);
	$('#maskRepMult').fadeIn(300);
}
//Here we implement the downloading and uploading of the parameters
//The parameters are stored in a txt file with the following rules: each line starting with # is a comment line, Each line that contains a valid variable name is followed by a line with the variables value
//In case a variable is an array the lines of the array are separated by double tabs and the rows by tabs
//The save file also supports commands starting with ! for more functionality
var SaveParams = function(output_file)
{
	//Save the parameters
	//First create as a string the contents of the text file
	var mytext = CreateParamsFile();

	if (mytext !== "")
	{
		var thedata = new FormData();
		thedata.append('expname', $('input[name="expid"]').val());
		thedata.append('texttodownload', mytext);
		thedata.append('session_id', sessionid);
		thedata.append('output', output_file);
		$.ajax({
		  url: cgi_bin_path + 'download_param_file.php', //Server script to fire up data analysis
		  type: 'POST',
		  // Form data
		  data: thedata,
		  //Options to tell jQuery not to worry about content-type.
		  processData: false,
		  cache: false,
		  contentType: false,
	   }).done(function (data, textStatus, jqXHR) {
		   if (output_file == 0)
		   {
			msgbox('Download your parameters using this link: <a href="' + data.results_url + '" download target="_blank">' + $('input[name="expid"]').val() + ' parameters.txt</a>.')
		   }
	   });
	}
}

var CreateParamsFile = function()
{
	var mytext = "";
	mytext += "#ProteoSign parameters files for " + $('input[name="expid"]').val() + " (session ID: " + sessionid + ")\n#\n#File Information:\n";
	mytext += "isLabelFree\n";
	mytext += isLabelFree + "\n";
	mytext += "isIsobaricLabel\n";
	mytext += isIsobaricLabel + "\n";
	mytext += "procprogram\n";
	var quantlabel = $("#quantitation_prog_lbl").text();
	var pattern = new RegExp("MaxQuant");
	var res = pattern.test(quantlabel);
	var provenprocprogram = "";
	if (res == true)
	{
		provenprocprogram = "MQ";
	}
	else
	{
		provenprocprogram = "PD";
	}
	mytext += provenprocprogram + "\n";
	mytext += "#User Parameters and commands:\n";
	mytext += "rawfiles_structure\n";
	var myval = "";
	var mytemp = "";
	   if (RMisused)
	   {
		   var temp_rf_structure = [];
		   var all_items = $('#rawfiles_tbl_allfiles').find('tr');
			for (var i = 1; i < all_items.length; i++) {
				var items_tds = $(all_items[i]).find('td');
				temp_rf_structure.push({rawfile: $(items_tds[0]).text(), biorep: "-", techrep: "-", fraction: "-", used: true});
		}
   }
   if (!RMisused)
   {
	$.each(rawfiles_structure, function(idx, my_rawfile)
	{
		if (my_rawfile.used == true)
		{
			mytemp = "1";
		}
		else
		{
			mytemp = "0";
		}
		myval += my_rawfile.rawfile + "\t" + my_rawfile.biorep + "\t" + my_rawfile.techrep + "\t" + my_rawfile.fraction + "\t" + mytemp + "\t\t";
	});
   }
   else
   {
	$.each(temp_rf_structure, function(idx, my_rawfile)
	{
		if (my_rawfile.used == true)
		{
			mytemp = "1";
		}
		else
		{
			mytemp = "0";
		}
		myval += my_rawfile.rawfile + "\t" + my_rawfile.biorep + "\t" + my_rawfile.techrep + "\t" + my_rawfile.fraction + "\t" + mytemp + "\t\t";
	});
   }
	mytext += myval + "\n";
	mytext += "expid\n";
	mytext += $('input[name="expid"]').val() + "\n";
	mytext += "!Rename\n";
	myval = "";
	$.each(RenameArray, function(idx, my_line)
	{
		myval += my_line[0] + "|" + my_line[1] + "||";
	});
	mytext += myval + "\n";
	mytext += "exptpoint\n";
	mytext += $('input[name="exptpoint"]').val() + "\n";
	mytext += "conditions_to_compare\n";//these are the authentic items in the conditions list
	myval = "";
	$.each(AuthenticItemsinRename, function(idx, my_cond)
	{
		myval += my_cond + "\t";
	});
	mytext += myval + "\n";
	mytext += "quantitation_filtering\n";
	if($("#expquantfilt").prop("checked") == true)
	{
		myval = "T";
	}
	else
	{
		myval = "F";
	}
	mytext += myval + "\n";
	mytext += "filtering_label\n";
	mytext += $("#expquantfiltlbl option:selected").val() + "\n";
	mytext += "peptide_level_filtering\n";
	if($("#expquantfiltprot").prop("checked") == true)
	{
		myval = "T";
	}
	else
	{
		myval = "F";
	}
	mytext += myval + "\n";
	mytext += "LeastBRep\n";
	mytext += LeastBRep + "\n";
	mytext += "LeastPep\n";
	mytext += LeastPep + "\n";
	mytext += "Pthreshold\n";
	mytext += Pthreshold + "\n";
	mytext += "LFQconditions\n";
	myval = "";
	$.each(RawFileConditions, function(idx, my_cond)
	{
		myval += my_cond.name + "\t" + my_cond.condition + "\t\t";
	});
	mytext += myval + "\n";
	mytext += "!LS_Array\n";
	myval = "";
	$.each(LabelSwapArray, function(idx, my_line)
	{
		myval += my_line[0] + "|" + my_line[1] + "|" + my_line[2] + "|" + my_line[3] + "||";
	});
	mytext += myval + "\n";
	mytext += "!LS_c_p_Add\n";
	myval = "";
	$.each(LS_counters_per_Add, function(idx, my_line)
	{
		myval += my_line[0] + "||"; //add the description of the ls assignment
		$.each(my_line[1], function(idx, my_index)//the array of indices in this assignment
		{
			myval += my_index + "|";
		});
		myval += "|";
		myval += my_line[2][0] + "||";
		myval += my_line[2][1] + "|||";
	});
	mytext += myval + "\n";
	mytext += "!Select_Labels\n";
	myval = "";
	$("#conditions_list option").each(function(idx, my_opt)
	{
		if ($(my_opt).prop("selected") == true)
		{
			myval += $(my_opt).val() + "|";
		}
	});
	mytext += myval + "\n";
	mytext += "!RMrawfilesdata\n";
	mytext += array_to_string(RMrawfilesdata) + "\n";
	mytext += "!RMtagsdata\n";
	mytext += array_to_string(RMtagsdata) + "\n";
	mytext += "!RMbrepsRepInRawFiles\n";
	mytext += RMbrepsRepInRawFiles + "\n";
	mytext += "!RMtrepsRepInRawFiles\n";
	mytext += RMtrepsRepInRawFiles + "\n";
	mytext += "!RMconditionsRepInRawFiles\n";
	mytext += RMconditionsRepInRawFiles + "\n";
	mytext += "!RMisused\n";
	mytext += RMisused + "\n";
	return mytext;
}

var getparamfile = function()
{
	$("#__upldparams").click();
}

var LoadParams = function(myparametersstring)
{
	//load the file
	var checkres = CheckParamsValidity(myparametersstring);
	if (checkres == "")
	{
		var additional_info = "";
		//load
		var paramslines = myparametersstring.split("\n");
		var setvar = "";
		var mypatt = new RegExp("#ProteoSign parameters files");
		var myres = mypatt.test(paramslines[0]);
		if (!myres)
		{
			msgbox("The uploaded file is not valid")
			return;
		}
		$.each(paramslines, function(idx, myparamline){
			if(myparamline[myparamline.length - 1] == "\r")
			{
				myparamline = myparamline.substring(0, myparamline.length - 1);
			}
			if (myparamline.charAt(0) == "#")
			{
				return;
			}
			if (setvar == "")
			{//in this case we expect to read a var name
				if (myparamline == "isLabelFree" || myparamline == "isIsobaricLabel" || myparamline == "procprogram" || myparamline == "rawfiles_structure" || myparamline == "expid" || myparamline == "exptpoint" || myparamline == "conditions_to_compare" || myparamline == "quantitation_filtering" || myparamline == "filtering_label" || myparamline == "peptide_level_filtering" || myparamline == "LeastBRep" || myparamline == "LeastPep" || myparamline == "Pthreshold" || myparamline == "LFQconditions" || myparamline == "!Rename" || myparamline == "!LS_Array" || myparamline == "!LS_c_p_Add" || myparamline == "!Select_Labels" || myparamline == "!RMrawfilesdata" || myparamline == "!RMtagsdata" || myparamline == "!RMbrepsRepInRawFiles" || myparamline == "!RMtrepsRepInRawFiles" || myparamline == "!RMconditionsRepInRawFiles" || myparamline == "!RMisused")
				{
					setvar = myparamline;
				}
			}
			else
			{
				if (setvar == "rawfiles_structure")
				{
					if (add_raw_file_structure(myparamline, false) == false)
					{
						//in this case some rawfiles in the dataset are not included in the parameter's	file
						additional_info = "<br>Some raw files where not matched to a biological coordinate"
						if (my_step == 3)
						{// if we are on steo 3 send the user back to step 2 to take care of the unset rawfiles
							toggleNextClass(idsToStrings(".main_div .main_section").reverse(), "hidden", true, rollbackStage);
							rawfiles_tbl_allfiles_DT.columns.adjust().draw();
						}
					}
				}
				else if(setvar == "expid")
				{
					$('input[name="expid"]').val(myparamline);
				}
				else if (setvar == "exptpoint")
				{
					$('input[name="exptpoint"]').val(myparamline);
				}
				else if (setvar == "conditions_to_compare")
				{
					var myvalues = myparamline.split("\t");
					AuthenticItemsinRename = [];
					$.each(myvalues, function(idx, myval){
						if (myval == "") return;
						AuthenticItemsinRename.push(myval);
					});
				}
				else if (setvar == "quantitation_filtering")
				{
					if (myparamline == "F")
					{
						$("#expquantfilt").prop("checked", false);
						//hide the rest
						$("#s3advparams select[name='expquantfiltlbl']").closest("tr").addClass("hidden");
						$("#s3advparams input[name='expquantfiltprot']").closest("tr").addClass("hidden");
					}
					else if (myparamline == "T")
					{
						$("#expquantfilt").prop("checked", true);
						//show them again
						 $("#s3advparams select[name='expquantfiltlbl']").closest("tr").removeClass("hidden");
						 $("#s3advparams input[name='expquantfiltprot']").closest("tr").removeClass("hidden");
						 var parentdiv = $(this).closest("div");
						 parentdiv.scrollTop(parentdiv.prop("scrollHeight"));
					}
				}
				else if (setvar == "filtering_label")
				{
					$('#expquantfiltlbl option:contains("' + myparamline + '")').prop('selected', true);
				}
				else if (setvar == "peptide_level_filtering")
				{
					if (myparamline == "F")
					{
						$("#expquantfiltprot").prop("checked", false);
					}
					else if (myparamline == "T")
					{
						$("#expquantfiltprot").prop("checked", true);
					}
				}
				else if (setvar == "LeastBRep")
				{
					LeastBRep = myparamline;
				}
				else if (setvar == "LeastPep")
				{
					LeastPep = myparamline;
				}
				else if (setvar == "Pthreshold")
				{
					Pthreshold = myparamline;
				}
				else if (setvar == "LFQconditions")
				{
					if (myparamline == "")
					{
						RawFileConditions = [];
						//Show everything back to the user
						var all_items = $('#rawfiles_tbl_allfiles').find('tr');
						// console.log(all_items);
						for (var i = 0; i < all_items.length; i++) {
						 var items_tds = $(all_items[i]).find('td');
						 var items_cond = items_tds[4];
						 $.each(RawFileConditions, function (idx, my_raw_file)
						 {
							 if (my_raw_file.name == $(items_tds[0]).text())
							 {
								 $(items_cond).text(my_raw_file.condition);
								 return false;
							 }
						 });
						}
						setvar = "";
						return;
					}
					add_LFQ_conditions(myparamline);
				}
				else if (setvar == "!Rename")
				{
					if (myparamline == "")
					{
						RenameArray = [];
						AuthenticItemsinRename = [];
						setvar = "";
						return
					}
					if(!AllowMergeLabels)
					{
						RenameArray = [];
						AuthenticItemsinRename = [];
						setvar = "";
						return
					}
					RenameArray = [];
					AuthenticItemsinRename = [];
					var my_val = myparamline;
					var lines = my_val.split("||");
					$.each(lines, function(idx, my_line){
						if (my_line == "") return;
						var my_values = my_line.split("|");
						RenameArray.push(my_values);
						AuthenticItemsinRename.push(my_values[0]);
					});
					var temprenamefromtestdata = RenameFromTestData;
					RenameFromTestData = true;
					Refresh_conds_list();
					RenameFromTestData = temprenamefromtestdata;
				}
				else if (setvar == "!LS_Array")
				{
					if (myparamline == "")
					{
						LabelSwapArray = [];
						setvar = "";
						return
					}
					if(!AllowLS) return;
					LabelSwapArray = [];
					var my_val = myparamline;
					var lines = my_val.split("||");
					$.each(lines, function(idx, my_line){
						if (my_line == "") return;
						var my_values = my_line.split("|");
						var to_add = [];
						to_add.push(my_values[0]);
						to_add.push(my_values[1]);
						to_add.push(my_values[2]);
						to_add.push(parseInt(my_values[3]));
						LabelSwapArray.push(to_add);
					});
					LS_array_counter = lines.length;
				}
				else if (setvar == "!LS_c_p_Add")
				{
					if (myparamline == "")
					{
						LS_counters_per_Add = [];
						$("#LSLabelSwaps").empty();
						setvar = "";
						return
					}
					if(!AllowLS) return;
					LS_counters_per_Add = [];
					var my_val = myparamline;
					var lines = my_val.split("|||");
					$("#LSLabelSwaps").empty();
					$.each(lines, function(idx, my_line){
						if (my_line == "") return;
						var my_values = my_line.split("||");
						var to_add = [];
						to_add[0] = my_values[0];
						to_add[1] = [];
						var seclines = my_values[1].split("|");
						$.each(seclines, function(idx, my_secline){
							to_add[1].push(parseInt(my_secline));
						});
						to_add[2] = [];
						to_add[2].push(my_values[2]);
						to_add[2].push(my_values[3]);
						LS_counters_per_Add.push(to_add);
						//Add the respective lines in LSLabelSwaps
						$("#LSLabelSwaps").append("<option value='" + my_values[0] + "'>" + my_values[0] + "</option>");
					});
				}
				else if (setvar == "!Select_Labels")
				{
					if (myparamline == "")
					{
						setvar = "";
						return
					}
					my_lbls_toselect = myparamline.split("|");
					select_labels_according_to_test_dataset();
					my_lbls_toselect = [];
				}
				else if (setvar == "!RMrawfilesdata")
				{
					RMrawfilesdata = string_to_array(myparamline);
				}
				else if (setvar == "!RMtagsdata")
				{
					RMtagsdata = string_to_array(myparamline);
				}
				else if (setvar == "!RMbrepsRepInRawFiles")
				{
					RMbrepsRepInRawFiles = (myparamline == 'true');
				}
				else if (setvar == "!RMtrepsRepInRawFiles")
				{
					RMtrepsRepInRawFiles = (myparamline == 'true');
				}
				else if (setvar == "!RMconditionsRepInRawFiles")
				{
					RMconditionsRepInRawFiles = (myparamline == 'true');
				}
				else if (setvar == "!RMconditionsRepInRawFiles")
				{
					RMconditionsRepInRawFiles = (myparamline == 'true');
				}
				else if (setvar == "!RMisused")
				{
					RMisused = (myparamline == 'true');
					if (RMisused)
					{
						set_RMisused(true);
						RM_init_RMsteps_from_load_data();
						reset_reps();
					}
					else
					{
						set_RMisused(false, false);
					}
				}
				setvar = "";
			}
		});
		msgbox("Loading completed." + additional_info);
	}
	else
	{
		msgbox("<p style='border-bottom: solid; text-align: center'><strong>The following errors occured when loading parameters:</strong></p><ol>" + checkres + "</ol>");
	}
}
var CheckParamsValidity = function(myparamsstring)
{
	var error_message = "";
	var paramslines = myparamsstring.split("\n");
	var setvar = "";
	$.each(paramslines, function(idx, myparamline){
		if(myparamline[myparamline.length - 1] = "\r")
		{
			myparamline = myparamline.substring(0, myparamline.length - 1);
		}
		if (myparamline.charAt(0) == "#")
		{
			return;
		}
		if (setvar == "")
		{//in this case we expect to read a var name
			if (myparamline == "isLabelFree" || myparamline == "isIsobaricLabel" || myparamline == "procprogram" || myparamline == "rawfiles_structure" || myparamline == "expid" || myparamline == "exptpoint" || myparamline == "conditions_to_compare" || myparamline == "quantitation_filtering" || myparamline == "filtering_label" || myparamline == "peptide_level_filtering" || myparamline == "LeastBRep" || myparamline == "LeastPep" || myparamline == "Pthreshold" || myparamline == "LFQconditions" || myparamline == "!Rename" || myparamline == "!LS_Array" || myparamline == "!LS_c_p_Add")
			{
				setvar = myparamline;
			}
		}
		else
		{//here we have the var name in set var and we read the var value
			if (setvar == "isLabelFree")
			{
				if (myparamline == "true")
				{
					if (isLabelFree == false)
					{
						error_message += "<li style='text-align: left;'>The parameters correspond to a labelled experiment set but the uploaded data set to a label-free<br></li>";
					}
				}
				else if (myparamline == "false")
				{
					if (isLabelFree == true)
					{
						error_message += "<li style='text-align: left;'>The parameters correspond to a label-free experiment set but the uploaded data set to labelled<br></li>";
					}
				}
			}
			else if (setvar == "isIsobaricLabel")
			{
				if (myparamline == "true")
				{
					if (isIsobaricLabel == false)
					{
						error_message += "<li style='text-align: left;'>The parameters correspond to an isobaric labelled experiment set but the uploaded data set does not<br></li>";
					}
				}
				else if (myparamline == "false")
				{
					if (isIsobaricLabel == true)
					{
						error_message += "<li style='text-align: left;'>The parameters correspond to an experiment which did not employ isobaric labeling but the uploaded data set corresponds to such an experiment<br></li>";
					}
				}
			}
			else if (setvar == "procprogram")
			{
				var myexppddata;
				var provenprocprogram = "";
				if ($("#s3expparams input[name='exppddata']").prop("checked") == false)
				{
					provenprocprogram = "MQ";
				}
				else
				{
					provenprocprogram = "PD";
				}
				if (myparamline == "MQ")
				{
					if (provenprocprogram == "PD")
					{
						error_message += "<li style='text-align: left;'>The parameters correspond to a data set processed by MaxQuant but the uploaded data set was processed by Proteome Discoverer<br></li>";
					}
				}
				else if (myparamline == "PD")
				{
					if (provenprocprogram == "MQ")
					{
						error_message += "<li style='text-align: left;'>The parameters correspond to a data set processed by Proteome Discoverer but the uploaded data set was processed by MaxQuant<br></li>";
					}
				}
			}
			else if (setvar == "rawfiles_structure")
			{
				if (add_raw_file_structure(myparamline, true) == false)
				{
					error_message += "<li style='text-align: left;'>The parameters correspond to a data set with different raw files than the uploaded data set<br></li>";
				}
			}
			setvar = "";
		}

	});
	return error_message;
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
   //toggleNextClass searches in all the step divs for the first div that does not have a hidden class (that is shown). it hides this step div and shows the next one.
   //the order of the step divs matters, if we want to step fwd, the divs are in ascending order (from s1div to s5div) otherwise they are ordered vice versa
   var nToggle = 0;
   var toggleClassFunction = (_removeClass ? ["removeClass", "addClass"] : ["addClass", "removeClass"]);
   var reachedLastItem = false;
   //typically: className = hidden (_removeClass is always true)
   itms.some(function (itm, index) {
      var cond = (_removeClass ? $(itm).hasClass(className) : !$(itm).hasClass(className));//cond is true when the div is hidden
      if (cond) {
         if (nToggle > 0) {//if nToggle > 0 then you have already hidden a step div so it is time to show the next one
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
      return (nToggle === 2);//if two items were toggled (one shown one hidden) exit the some function
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
   $("#results_p").html("Now analysing your data. Please wait for the results.<p style='font-size: 14px; margin-top: 8px; margin-bottom: 0;'>Or press <i>Back</i> to stop the analysis and change your parameters.</p>");
}

var postFireUpAnalysisAndWait = function () {
	//before initiating the analysis create a save parameters file to the output folder
	SaveParams(1);
	//also make sure that the analysis_finished var is false since we are initiating a new analysis
	analysis_finished = false;
	RreturnedErrorsInCurSession = false;
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   thedata.append("AllowMergeLabels", AllowMergeLabels ? "T" : "F");
   thedata.append("AllowLS", AllowLS ? "T" : "F");
   	//get the proc program (MQ or PD)
	var quantlabel = $("#quantitation_prog_lbl").text();
	var pattern = new RegExp("MaxQuant");
	var res = pattern.test(quantlabel);
	var provenprocprogram = "";
	if (res == true)
	{
		provenprocprogram = "MQ";
	}
	else
	{
		provenprocprogram = "PD";
	}
	thedata.append('proc_program', provenprocprogram);
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
         getRSS("https://academic.oup.com/nar/pages/Top_Articles_Data_Resources", "#server_feedback");
      },
   }).done(function (data, textStatus, jqXHR) {
	  if (data.ret_session != sessionid)// the statement is checked to be true so that in the unlikely event of two procedures running from the same session only one should give results, the one with the correct session id
	  {
		  return;
	  }
	  var R_returned_error = false;
	  //$("#s4btnb").prop('disabled', true);
	  if(sectionSpinnerOn)
	  {
		toggleCurrentSectionSpinner();
	  }
      $("#server_feedback").empty();
      $("#s4btnf").prop('disabled', !data.success);
	  //Now display the errors and warnings of R to the user (if any)
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
		  UserInfoDisplay = UserInfoDisplay.replace(/Warn User: /g, '<p style="color: #BA4A00">')
		  UserInfoDisplay = UserInfoDisplay.replace(/Error User: /g, '<p style="color: #E60000">')
		  UserInfoDisplay = UserInfoDisplay.replace(/Info User: /g, '<p>')
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
	  //if everything went fine show the results:
      if (data.success) {
         $("#results_p").html("Now you can inspect your results. When ready, click <em>Next</em>.<br><br>");
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
		 //make the server feedback aesthetically nice:
		 $("#server_feedback").css("box-shadow", "0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19)")
      } else {
         $("#results_p").html("");
         $("#server_feedback").html("<span class='uploadErrorMsg'><strong><em>The analysis could not be completed: " + data.msg + "<em><strong></span>");
         if (data.R_dump.length > 0) {
            $("#server_feedback").append("<br><br><span style='font-family: Georgia; font-size: 95%; text-align: left; display: inline-block; width: 90%'><p>Please ensure that input parameters (such as number of replicates, number of biological conditions/labels etc) are correctly defined and input data format is valid. You can press <i>Back</i> to change your parameters.</p><p>If the above does not apply, then the statistical analysis may have failed due to numerical problems (e.g. too many missing values).</p><p> If you feel that none of the above is the case, please click <a href='mailto:msdiffexp@gmail.com?Subject=Session%20" + sessionid + "' target='_blank'><u>here</u></a> to notify via e-mail the ProteoSign team or <a onclick='onFeedbackclick();'><u>here</u></a> to leave us a feedback.</p></span>");
         }
		 R_returned_error = true;
		 RreturnedErrorsInCurSession = true;
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
      $("#scrollingtext").html("Welcome to ProteoSign");
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
   if (RMisused)
   {
	   var temp_rf_structure = rawfiles_structure.slice();
	   rawfiles_structure.push({rawfile: "RM_exmpl1", biorep: "1", techrep: "1", fraction: "1", used: "1"});
	   rawfiles_structure.push({rawfile: "RM_exmpl2", biorep: "2", techrep: "1", fraction: "1", used: "1"});
   }
   thedata.append("exp_struct", gen_expdesign(rawfiles_structure));
   if (RMisused)
   {
	   rawfiles_structure = temp_rf_structure.slice();
   }
   thedata.append("LFQ_conds", gen_lfqdesign(RawFileConditions));
   thedata.append("LeastBreps", LeastBRep);
   thedata.append("LeastPeps", LeastPep);
   thedata.append("PThreshold", Pthreshold);
   if(AllowMergeLabels) thedata.append("Rename_Array", gen_RenameFile(RenameArray));
   if(AllowLS) thedata.append("LabelSwapArray", gen_LSArray(LabelSwapArray));
   //Deal with the RM vars:
   thedata.append("RMRawFiles", generate_tab_file(RMrawfilesdata));
   thedata.append("RMTags", generate_tab_file(RMtagsdata));
   thedata.append("RMbrepsRepInRawFiles", RMbrepsRepInRawFiles ? "T" : "F");
   thedata.append("RMtrepsRepInRawFiles", RMtrepsRepInRawFiles ? "T" : "F");
   thedata.append("RMconditionsRepInRawFiles", RMconditionsRepInRawFiles ? "T" : "F");
   thedata.append("RMisused", RMisused ? "T" : "F");
   //end: RM vars
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
         msgbox("ERROR on SERVER: " + data.msg);
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
   //Warning! the following lines work only if ther are 6 steps (1, 2, 22, 3, 4, 5) in ProteoSign! if we add another step, change the indices:
   switch (stageIndex) {
    case 1:
		//if we go back to step 1 reset the state so that the uploading of files goes to a new folder
         resetState();
		 break;
	case 3:
		//if we go back to step 3 reset the session (copy the uploaded files and start a new session)
		 resetSession();
		 break;
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
		url: cgi_bin_path + 'change_session.php', //Server script to change the session
		type: 'POST',
		// Form data
		data: thedata,
		//Options to tell jQuery not to worry about content-type.
		processData: false,
		cache: false,
		contentType: false
	}).done(function (data, textStatus, jqXHR) {
	});
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
	if(!isIsobaricLabel)
	{
		var error_to_display = "";
		var re = new RegExp('^(?![0-9_])[a-zA-Z0-9_]+$');
		$("#conditions_list option").each(function(idx, my_opt)
		{
			if (!re.test($(this).val()))
			{
				error_to_display = error_to_display + "Condition name " + $(this).val() + " contains invalid characters or starts with a number or an underscore, please rename the condition to continue<br>"
			}
		});
		if (error_to_display !== "")
		{
			msgbox(error_to_display);
			nInvalid++;
		}
	}
   return (nInvalid == 0 && tmp_i > 1 && found_cond);
}

// reset "counters"/states reset state(false) is called whenever the program starts from the very beggining
var resetState = function (uploading_new_file) {
   uploading_new_file = (typeof uploading_new_file !== 'undefined') ? uploading_new_file : false;
   unsuccessfullyUploadedFiles = {};
   uploadEfforts = {};
   if (!$("#explbl1definition").hasClass("hidden")) {
      $("#explbl1definition").addClass("hidden");
   }
	if(uploading_new_file == false)
	{
		// Reset items that contained information from previous data input files (e.g. label information)
		$("#step2ToolsWrapper").css({"display": "inline-flex"});
		$("#step2RMuseinfo").css({"display": "none"});
		$("#step2InfoHeader").css({"display": "block"});
		$("#step2desc").css({"display": "block"});
		$("#s3expparamsDlgLabelsSelection").empty();
		$("#s3div h2").html("Step 3 ");
		$("#explbl1name_").empty();
		$("#explbl0name_").empty();
		$("#s3advparams select[name='expquantfiltlbl']").empty();
		$("#quantsoftdetectedspan").empty();
		$("#s22btnf").prop('disabled', true);
		$("#s3showdivLabelSwapRow").show();
		$("#s3QuantitationFiltering").show();
		rawfiles = undefined;
		AllowMergeLabels = true;
		AllowLS = true;
		$('#conds_list_container').contextmenu("option", "autoTrigger", AllowMergeLabels);
		procProgram = "";
		peptideLabelsFromFile = [];
		peptideLabelsFromFileCombs = [];
		peptideLabelsNamesFromFile = [];
		RM_RMstep = 1;
		RMrawfilesdata = [];
		RMtagsdata = [];
		RMbrepsRepInRawFiles = true;
		RMtrepsRepInRawFiles = true;
		RMconditionsRepInRawFiles = true;
		RMtags = [];
		RMisused = false;
		stepRM2initialized = false;
		stepRM3initialized = false;
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
		  //In case the sessionid changed in session1 do not use the file in ProteoSign
		  if (data.ret_session != sessionid)
		  {
			return;
		  }
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
            // 1. Non-labelled "background" species present? (obsolete)
            var item1 = $("#s3advparams input[name='explbl00']").closest("tr").children().first();
            // 2. Quantitation filtering?
            var item2 = $("#s3advparams input[name='expquantfilt']").closest("tr").children().first();
            // 3. Species label #1 (obsolete)
            var item3 = $("#explbl1name_").closest("tr").children().first();
            // 4. Species label #1 select item td (obsolete)
            var item4 = $("#explbl1name_").closest("td");
            // 5. Label tooltip (obsolete)
            var item5 = $("#quantsoftdetectedspan").closest("td").children().first();
            // 6. Species label #1 definition (obsolete)
            var item6 = $("#explbl1definition");
            //Experiment type detection:
			//the following lines detect the experiment type of the dataset: upload_files.php returns one array called peptide_labels_names, which is full of data only if the dataset is a labelled one (precursor ion or isobaric label) and empty if it is label-free. In case of labelled data, the array peptide_labels is full only in PD data and empty in MQ data
			//Another script called get_labels_to_js will be executed afterwards to distinguish the two labeled types.
            if (peptideLabelsNamesFromFile.length > 0) {
			   //Labeled case:
			   // Set states of parameters relevant to labelled experiments accordingly (all items but item2 are obsolete)
               $(item1).prop('disabled', false);
               $(item2).prop('disabled', false);
               $(item3).html('&#8212 Species label #1');
               $(item4).html('<select data-required="true" id="explbl1name_" name="explbl1name" type="text" placeholder="Name"></select>');
               // Only Proteome Discoverer data currently provide label definition information.
               // When this information is made available it means that our data originate from PD software.
               $("#s3expparams input[name='exppddata']").prop('checked', peptideLabelsFromFile.length > 0);
			   
               //<img class="callout" src="../images/callout_black.gif" /><strong>Warning!</strong><br><u>The order of labels defined here matters</u>. Define your labels in the same order they were defined in <a id="quantsoftdetectedspan"></a>. If there exist unlabeled species, please define them in the <em>advanced parameters</em> section below.
			   
			   $("#quantsoftdetectedspan").text(peptideLabelsFromFile.length > 0 ? "Proteome Discoverer" : "MaxQuant"); //this item is obsolete
			   //Refresh the quantitation_prog_lbl that informs the user of the dataset's quantitation program (MQ or PD), this functionality is done in a slightly different way for Label Free data when the user presses the Next button in step 2.
               $("#quantitation_prog_lbl").text(peptideLabelsFromFile.length > 0 ? "\u2014 Raw data were quantified with Proteome Discoverer\u2122" : "\u2014 Raw data were quantified with MaxQuant");
               $(item5).removeClass('hidden');
               $(item6).attr('placeholder', 'Definition');
			   //hide the right click option "assign condition" in raw files table (step 2) and the extra info in step's 2 Caption, also hide the "Condition" column in step 2 and refresh the isLabelFree variable
			   $(document).contextmenu("showEntry", "assign_condition", false);
			   $("#ExtraInfoForLFQ").hide();
			   rawfiles_tbl_allfiles_DT.column("4").visible(false);
			   isLabelFree = false;
			   //Add here more functionality in case of Labeled data:
			   
            } else {
			   //Label-free case (disable non-applicable parameters and rename others accordingly)
			   //Note: some Isobaric Labelled datasets are considered mistakenly Label-Free. We will correct this in get_labels_to_js execution.
			   //all following items except item2 are obsolete:
               $(item1).prop('disabled', true);
               $(item2).prop('disabled', true);
               $(item3).html('&#8212 Biological condition #1');
               $(item4).html('<input data-required="true" id="explbl1name_" name="explbl1name" type="text" onkeypress="inputChCheck(event,\'^(?!_)[a-zA-Z0-9_]+$\',20)" placeholder="Character rules apply"></input>');
               $(item5).addClass('hidden');
               $(item6).attr('placeholder', 'Raw file');
			   //show the right click option "assign condition" in raw files table (step 2) and the extra info in step's 2 Caption, also show the "Condition" column in step 2 and refresh the isLabelFree variable
			   $("#ExtraInfoForLFQ").show();
			   $(document).contextmenu("showEntry", "assign_condition", true);
			   rawfiles_tbl_allfiles_DT.column("4").visible(true);
			   isLabelFree = true;
			   //Add here more functionality in case of LFQ data:
			   
            }

            // $.each(peptideLabelsFromFile, function (idx, lbl_i) {
			   // $("#conditions_list").append("<option value='" + lbl_i + "' style='padding: 2px; font-size: 125%;' selected='true'>" + lbl_i + "</option>");
               // $("#s3expparamsDlgLabelsSelection").append("<option value='" + lbl_i + "'>" + lbl_i + "</option>");
            // });
			//AddedLabels is false when beggining uploading a new dataset, its purpose is to make sure that the conditions list will get full of the dataset's condition only once
			//The following lines apply only in labelled datasets, LabelFree datasets' conditions are defined by the user and added in the conditions list when he/she presses the button Next in Step 2
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
			   //addFormLabel will execute get_labels_to_js:
				addFormLabel();
			   AddedLabels = true;
			}

            if (peptideLabelsFromFile.length > 0) {
				//obsolete:
               $("#explbl1definition").removeClass("hidden");
            }
            while (nFormLabels > 1) {
			   //obsolete:
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

var on_exppddatachange = function()
{
	//called when the program understands that the dataset is from PD or MQ
	// this function serves the only reason to inform
	//the user for the procprogram (MQ or PD) by altering the quantitation_prog_lbl
	// this function will run only in label-free data since in labelled ones,
	//quantitation_prog_lbl is set in another way
	if (isLabelFree)
	{
		if ($("#s3expparams input[name='exppddata']").prop("checked") == false)
		{
			//MaxQuant:
			$("#quantitation_prog_lbl").text("\u2014 Raw data were quantified with MaxQuant");
		}
		else
		{
			//Proteome Discoverer:
			$("#quantitation_prog_lbl").text("\u2014 Raw data were quantified with Proteome Discoverer\u2122");
		}
	}
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

var ons2btnfclick = function()
{
	//if the user successfuly entered second step log the following information to a php log file
	var provenprocprogram = "";
	var typeofdataset = "";
	if (document.getElementsByName("exppddata")[0].checked)
	{
		provenprocprogram = "PD";
	}
	else
	{
		provenprocprogram = "MQ";
	}
	if (isLabelFree)
	{
		typeofdataset = "LabelFree";
	}
	else
	{
		if (isIsobaricLabel)
		{
			typeofdataset = "IsobaricLabel";
		}
		else
		{
			typeofdataset = "PrecursorIon";
		}
	}
	var toappend = "ProcProgram: " + provenprocprogram + "\n" + "ExperimentType: " + typeofdataset + "\n" + "ReachedStep2: T\n" + "IP: " + clientname + "\n" + "Test_dataset: " + (log_test_dataset ? "T" : "F") + "\n";
	log_test_dataset = false;
	log_php(toappend);
	$("#s2btnupld").prop('disabled', false);//when leaving step 1 make sure the upload button comes back enabled (it may have been disabled if the user chose a test dataset)
}

var log_php = function(texttoappend)
{
	//This function appends the specified text to pho lig file of the current session
	var thedata = new FormData();
	thedata.append('texttoappend', texttoappend);
	thedata.append('session_id', sessionid);
	$.ajax({
	  url: cgi_bin_path + 'log_php.php', //Server script to send the feedback
	  type: 'POST',
	  // Form data
	  data: thedata,
	  //Options to tell jQuery not to worry about content-type.
	  processData: false,
	  cache: false,
	  contentType: false,
   });
}
var ons22btnfclick = function(e)
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
		on_exppddatachange();
		//since we updated the conditions remove invalid label swaps:
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
			$("#LSLabelSwaps option").each(function(idx, my_opt)
			{
				if ($.inArray($(my_opt).val(), my_invalid_LSwaps) != -1)
				{
					$(my_opt).prop("selected", true);
					real_invalid_LSs++;
				}
				else
				{
					$(my_opt).prop("selected", false);
				}
			});
			if (real_invalid_LSs > 0)
			{
				onLSRemoveclick();
			}
		}
	}
}
var onShowDialog = function (selector){
   $(".expparamsDlg").css({"left": ($("body").width() / 2) - ($(selector).width() / 2)});
   $('body').append('<div id="mask"></div>');
   $(selector).fadeIn(300);
   //$('#mask').fadeIn(300);   
}

var display_star_score = function()
{
	if (!stars_enabled) return;
	for (var i = 1; i <= 5; i++)
	{
		var my_id = "#star" + i;
		if (i <= star_hovered)
		{
			$(my_id).css("opacity", "1");
		}
		else
		{
			$(my_id).css("opacity", "0.6");
		}
	}
}

var addFormLabel = function()
{
	//This function executes get_labels_to_js which makes sure that the labels are in the correct order (necessary for MQ data) and checks if the dataset is IsobaricLabel
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
	   
	   //Rearrange the dataset's labels (necessary for MQ datasets):
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
			//The data from get_labels_to_js have the labels in correct order:
			peptideLabelsNamesFromFile = data.labels;
		}
		
		
		if (peptideLabelsNamesFromFile.length > 0) {
			//Labeled case
			//Refresh the conditions list:
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
		  if(data.isIsobaricLabel) isIsobaricLabel = true;
		  $("#s3QuantitationFiltering").show();
	   } else {
		//label-free case
		  if (peptideLabelsFromFile.length > 0) {
			  //The following lines fill up the conditions_list with the raw files of the label free dataset, the list will be refreshed with the user-defined conditions when he/she presses the Next button in Step 2
			  //So, the following lines are obsolete (up to </obsolete>):
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
			  //</obsolete>
			  
			  //hide the advanced parameters (Quantitation filtering) in case we have label-free data
			  $("#s3QuantitationFiltering").hide();			  
			  //The following lines are executed only in the infrequent situation when an Isobaric Labelled dataset is mistakenly considered as Label-Free.
			  if(data.isIsobaricLabel)
			  {
					//Here an Isobaric Labelled dataset was mistakenly considered a Label Free one:
					//Correct the wrong actions that took place:
					isIsobaricLabel = true;
					isLabelFree = false;
					$("#s3QuantitationFiltering").show();
					var item2 = $("#s3advparams input[name='expquantfilt']").closest("tr").children().first();
					$(item2).prop('disabled', false);// For quantitation filtering
					$("#ExtraInfoForLFQ").hide();
					$(document).contextmenu("showEntry", "assign_condition", false);
					rawfiles_tbl_allfiles_DT.column("4").visible(false);
					//in this case, refresh the conditions_list with the special list of conditions called "special_IL_labels" (IL: Isobaric Label) coming from get_labels_to_js:
					peptideLabelsNamesFromFile = data.special_IL_labels;
					peptideLabelsFromFile = [];
					//Refresh the conditions list:
					$("#conditions_list").empty();
					$("#s3advparams select[name='expquantfiltlbl']").empty();
					if(!AllowMergeLabels)
					{
						$.each(peptideLabelsNamesFromFile, function (idx, lblname_i) {
							if (StringisNumber(lblname_i))
							{
								peptideLabelsNamesFromFile[idx] = parseInt(lblname_i);
								lblname_i = parseInt(lblname_i);
							}
							$("#conditions_list").append("<option value='" + lblname_i + "' style='padding: 2px; font-size: 125%;' selected='true'>" + lblname_i + "</option>");
							$("#s3advparams select[name='expquantfiltlbl']").append("<option oncontextmenu='return false;' value='" + lblname_i + "'>" + lblname_i + "</option>");
						});
					}
					else
					{
						if (!RenameFromTestData)
						{
							$.each(peptideLabelsNamesFromFile, function (idx, lblname_i) {
								if (StringisNumber(lblname_i))
								{
									peptideLabelsNamesFromFile[idx] = parseInt(lblname_i);
									lblname_i = parseInt(lblname_i);
								}
								$("#conditions_list").append("<option value='" + lblname_i + "' style='padding: 2px; font-size: 125%;' selected='true'>" + lblname_i + "</option>");
								$("#s3advparams select[name='expquantfiltlbl']").append("<option oncontextmenu='return false;' value='" + lblname_i + "'>" + lblname_i + "</option>");
							});
							InitializeRename();
						}
					}
				select_labels_according_to_test_dataset();
				create_my_all_mq_labels(); 
			  }
		  }
	   }
	   	//This is a crucial point in the code, here we know if the dataset is labelfree, labelled or isobarically tagged
		if (isIsobaricLabel)
		{
			$(document).contextmenu("showEntry", "rep_mult", true);
		}
		else
		{
			$(document).contextmenu("showEntry", "rep_mult", false);
		}
	   // label-free case (obsolete):
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
         msgbox("ERROR on SERVER: " + data.msg);
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
					  $(tds[0]).css("text-decoration", "line-through");
					  $(tds[1]).css("text-decoration", "line-through");
					  $(tds[2]).css("text-decoration", "line-through");
					  $(tds[3]).css("text-decoration", "line-through");
					  if (isLabelFree == true) $(tds[4]).css("text-decoration", "line-through");
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
      msgbox("An AJAX error occurred: " + errorThrown);
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
         msgbox("ERROR on SERVER: " + data.msg);
      } else if(data.queryres.desc) {
         var i = 1;
         $.each(data.queryres.desc, function (idx, dataset_desc)
         {
            $("#s1TestDatasetsSelection").append("<option value='" + (i++) + "'>" + dataset_desc + "</option>");
         });
      }

   }).fail(function (jqXHR, textStatus, errorThrown) {
      msgbox("An AJAX error occurred: " + errorThrown);
   });
}

//the following recursive function transforms an array to a single-line string notice that both this and the following function require that their inputs do not contain the | and the ~ characters
var array_to_string = function(my_array, curdimension)
{
	var retstring = "";
	var curdelimiter = "|";
	if (typeof(curdimension) === 'undefined') curdimension = 1;
	if (curdimension > 15) return;
	curdelimiter = curdelimiter.repeat(curdimension);
	for (var i = 0 ; i <= my_array.length - 1; i++)
	{
		if (my_array[i].constructor === Array)
		{
			retstring += array_to_string(my_array[i], curdimension + 1) + curdelimiter;
		}
		else
		{
			retstring += my_array[i] + curdelimiter;
		}
	}
	retstring = retstring.substring(0, retstring.length - curdelimiter.length);
	return retstring;
}

//the following recursive function turns a string created by array_to_string back to an array
var string_to_array = function(my_string, curdimension)
{
	var curdelimiter = "~";
	var olddelimiter = "|";
	if (typeof(curdimension) === 'undefined')
	{
		curdimension = 1;
		var mycounter = 1;
		for (var i = 15; i > 0; i--)
		{
			var repeated_delimiter = olddelimiter.repeat(i);
			if (my_string.includes(repeated_delimiter))
			{
				var temp_regex = new RegExp(("\\" + olddelimiter).repeat(i), "g");
				my_string = my_string.replace(temp_regex, curdelimiter.repeat(mycounter++));
			}
		}
		mycounter--;
		curdimension = mycounter;
	}
	var retarray = [];
	var temp_array = my_string.split(curdelimiter.repeat(curdimension));
	for (var i = 0 ; i <= temp_array.length - 1; i++)
	{
		if (curdimension != 1 && temp_array[i].includes(curdelimiter.repeat(curdimension - 1)))
		{
			retarray.push(string_to_array(temp_array[i], curdimension - 1));
		}
		else
		{
			retarray.push(temp_array[i]);
		}
	}
	return retarray;
}

var bioreps;
var techreps;
var fractions;
var rawfiles;
var rawfiles_structure;
var rep_counts;
var lastclicked_rawfiles_tbl_tr = null;
var lastclicked_RMrawfiles_tbl_tr = null;

function add_raw_file_structure(tab_sep_string, check_validity)
{
	//This function prints all file names if tab_rep_string == "" or sets the rawfile_structure otherwise
	if(typeof(tab_sep_string) == "undefined" || tab_sep_string == "")
	{
		var all_items = $('#rawfiles_tbl_allfiles').find('tr');
		for (var i = 0; i < all_items.length; i++) {
			var items_tds = $(all_items[i]).find('td');
			//console.log($(items_tds[0]).text() + "\n");
		}
		return;
	}
	var all_items = $('#rawfiles_tbl_allfiles').find('tr');
	var local_rawfiles = [];
	for (var i = 0; i < all_items.length; i++) {
		var items_tds = $(all_items[i]).find('td');
		local_rawfiles.push($(items_tds[0]).text());
	}
	//local_rawfiles contains the rawfiles of the uploaded dataset
	if (!check_validity) rawfiles_structure = [];
	// if check_validity == true then we are just making sure that there is no rawfile in the param save file that does not correspond to any files in the dataset
	//also we accept that some rawfiles in te dataset do not have a match in the param save file but not all of them
	var lines = tab_sep_string.split("\t\t");
	var validityresponse = true;
	$.each(lines, function(idx, my_line){
		if (my_line == "") return;
		var my_vals = my_line.split("\t");
		if(my_vals[0] == "rawfile")
		{
			return;
		}
		if (check_validity == true)
		{
			//in this case the only reason to run the function is to check if all the rawfiles in the file uploaded exist in the current dataset
			if($.inArray(my_vals[0], local_rawfiles) == -1)
			{
				//if one rawfile in the params file is not in the dataset abort
				validityresponse = false;
			}
		}
		else
		{
			// if check validity == false then we should renew the rawfiles structure object
			rawfiles_structure.push({rawfile: my_vals[0], biorep: my_vals[1], techrep: my_vals[2], fraction: my_vals[3], used: my_vals[4]});
		}
	});
	//uptohere validityresponse is true if all rawfiles in the param save file have a match in the dataset
	if (check_validity)
	{
		return validityresponse;
	}
	//From now on check validity == false:
	//Here the validation has ended and we know that there is at least one rawfile in the
	//param file that matches one in the dataset. Update these and return false if there are some rawfiles in the dataset left unset
	var rawfilesset = []; //dataset's raw files that will be set
	var all_items = $('#rawfiles_tbl_allfiles').find('tr');
	for (var i = 0; i < all_items.length; i++) {
		 var items_tds = $(all_items[i]).find('td');
		$.each(rawfiles_structure, function (idx, my_raw_file)
		 {
			 if (my_raw_file.rawfile == $(items_tds[0]).text())
			 {
				 rawfilesset.push(my_raw_file.rawfile);
				 return false;
			 }
		 });
	}
	var rawfilesunset = []; //dataset's raw files that will not be set
	for (var i = 0; i < all_items.length; i++)
	{
		var items_tds = $(all_items[i]).find('td');
		if ($.inArray($(items_tds[0]).text(), rawfilesset) == -1)
		{
			if ($(items_tds[0]).text() != "") rawfilesunset.push($(items_tds[0]).text());
		}
	}
	var found_unset = 0;
	for (var i = 0; i < all_items.length; i++) {
		 var items_tds = $(all_items[i]).find('td');
		 var items_brep = items_tds[1];
		 var items_trep = items_tds[2];
		 var items_frac = items_tds[3];
		 if ($(items_tds[0]).text() == "") continue;
		 if ($.inArray($(items_tds[0]).text(), rawfilesunset) != -1)
		 {//if found unset raw in table create an instance of it, it means that this raw was not set at the time of creating the param file. So set the raw back to its original value (-,-,-,used)
			 $(items_brep).text("-");
			 $(items_trep).text("-");
			 $(items_frac).text("-");
			 $(items_tds[0]).css("text-decoration", "none");
			 $(items_tds[1]).css("text-decoration", "none");
			 $(items_tds[2]).css("text-decoration", "none");
			 $(items_tds[3]).css("text-decoration", "none");
			 if (isLabelFree == true)
			 {
				 $(items_tds[4]).css("text-decoration", "none");
			 }
			 found_unset++;
		 }
	}
	//Show everything back to the user
		all_items = $('#rawfiles_tbl_allfiles').find('tr');
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
					 $(items_tds[0]).css("text-decoration", "line-through");
					 $(items_tds[1]).css("text-decoration", "line-through");
					 $(items_tds[2]).css("text-decoration", "line-through");
					 $(items_tds[3]).css("text-decoration", "line-through");
					 if (isLabelFree == true)
					 {
						 $(items_tds[4]).css("text-decoration", "line-through");
					 }
				 }
				 else{
					 $(items_tds[0]).css("text-decoration", "none");
					 $(items_tds[1]).css("text-decoration", "none");
					 $(items_tds[2]).css("text-decoration", "none");
					 $(items_tds[3]).css("text-decoration", "none");
					 if (isLabelFree == true)
					 {
						 $(items_tds[4]).css("text-decoration", "none");
					 }
				 }
				 return false;
			 }
		 });
		}
		refresh_fractions();
		set_s22btnf();
		return !(found_unset > 0);
}

var add_LFQ_conditions = function(tab_sep_string)
{
	RawFileConditions = [];
	var lines = tab_sep_string.split("\t\t");
	$.each(lines, function(idx, my_line){
		if (my_line == "") return;
		var my_vals = my_line.split("\t");
		if(my_vals[0] == "rawfile")
		{
			return;
		}
		RawFileConditions.push({name: my_vals[0], condition: my_vals[1]});
	});
	
		//Show everything back to the user
		var all_items = $('#rawfiles_tbl_allfiles').find('tr');
		// console.log(all_items);
		for (var i = 0; i < all_items.length; i++) {
         var items_tds = $(all_items[i]).find('td');
         var items_cond = items_tds[4];
		 $.each(RawFileConditions, function (idx, my_raw_file)
		 {
			 if (my_raw_file.name == $(items_tds[0]).text())
			 {
				 $(items_cond).text(my_raw_file.condition);
				 return false;
			 }
		 });
		}
		refresh_fractions();
		set_s22btnf();
		refresh_LFQ_conditions_from_test_data();
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
			var all_LFQ_labels_found = [];
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
							all_LFQ_labels_found.push(my_cond.condition);
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
			all_LFQ_labels_found = ArrNoDupe(all_LFQ_labels_found);
			//Here if all used files have a corresponding label (condition) assigned all_files_have_label_assigned is set to true otherwise to false
			$("#s22btnf").prop('disabled', !(rawfiles.length > 0 && rawfiles_structure.length == rawfiles.length && all_files_have_label_assigned == true && found_used_rec && bioreps_used.length > 1 && all_LFQ_labels_found.length > 1));
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
   // console.log("ret: " + ret);
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

var generate_tab_file = function(my_array)
{
	//this function gets a 2 dimensional array and transforms it into a tabular file
	var ret = "";
	for(var i = 0; i <= my_array.length - 1; i++)
	{
		for (var j = 0; j <= my_array[i].length - 1; j++)
		{
			ret += my_array[i][j] + "\t";
		}
		ret = ret.substr(0, ret.length - 1);
		ret += "\n";
	}
	ret = ret.substr(0, ret.length - 1);
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
      prev_html = '<p>NAR Top Articles - Data Resources and Analyses:<br><br><a href="' + data[item] + '" target="_blank"><strong>' + item + '</strong></a></p>';
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
      url: cgi_bin_path + 'get_NAR_articles.php',
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
	   if (analysis_finished || RreturnedErrorsInCurSession) return; //in case the rss delays make sure that the r script has not already returned any errors and that the results are not yet displayed
	   var htmlcode = data.html_code;
	   //get the div where NAR stores their current articles
	   var match =  htmlcode.match(/<strong>([\s\S]+?)<\/a>/gi);
	   var jsonData = {};
	   $.each(match, function(idx, my_match)
	   {
		var mytempmatch = my_match.match(/<strong>(.+?)<\/strong>/i);
		var mytitle = mytempmatch[1];
		//clear the title from formats
		mytitle = mytitle.replace(/<.*?>/g, "");
		var mytempmatch2 = my_match.match(/<a href="(.+?)">/i);
		var myurl = mytempmatch2[1];
		jsonData[mytitle] = myurl;
	   });
	   renderRSSData(jsonData, renderelem)
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
var dlgFadeoutRepMult = function () {
   $(".RepMultDlg").fadeOut(300, function () {
      $('#maskRepMult').remove();
   });
}
var dlgFadeoutFeedback = function () {
   $(".expparamsDlgFeedback").fadeOut(300, function () {
      $('#maskFeedback').remove();
   });
}

var CarefulBack = function()
{
	//in case of step 2 and step 4 (and if the analysis has been finished) the user must be warned that if they step back they might lose their progress (they should upload new files or rerun the analysis)
	if (carefulBackStep == 2)
	{
		$("#CarefulBackTitle").empty();
		$("#CarefulBackTitle").append("<p style='font-size: 85%'>Going back to Step 1 requires that you upload files again.<br>Are you sure you want to proceed?</p>");
		$(".expparamsDlg").css({"left" : ($("body").width()/2) - ($("#WarnCarefulBack").width()/2)});
		$('body').append('<div id="mask"></div>');
		$("#WarnCarefulBack").fadeIn(300);
		$('#mask').fadeIn(300);
	}
	else if (carefulBackStep == 4)
	{
		if (analysis_finished)
		{
			$("#CarefulBackTitle").empty();
			$("#CarefulBackTitle").append("<p style='font-size: 85%'>Going back to Step 3 will start a new session.<br>Please make sure you downloaded your results.<br>Are you sure you want to proceed?</p>");
			$(".expparamsDlg").css({"left" : ($("body").width()/2) - ($("#WarnCarefulBack").width()/2)});
			$('body').append('<div id="mask"></div>');
			$("#WarnCarefulBack").fadeIn(300);
			$('#mask').fadeIn(300);
		}
		else
		{
			toggleNextClass(idsToStrings(".main_div .main_section").reverse(), "hidden", true, rollbackStage);
			rawfiles_tbl_allfiles_DT.columns.adjust().draw();
		}
	}
}

var onCarefulBackYesclick = function()
{
	//if the user decided to go back one step do it:
	toggleNextClass(idsToStrings(".main_div .main_section").reverse(), "hidden", true, rollbackStage);
	rawfiles_tbl_allfiles_DT.columns.adjust().draw();
	//and also close the popup window
	dlgFadeout();
}

var onCarefulBackNoclick = function()
{
	dlgFadeout();
}

var label_context_menu;

//The following routines are for Replication Multiplexing
var onRMDialogNext_click = function()
{
	//if the user already uses Replication Multiplexing and he has not changed his options in step RM1 then the tables in RM2 and RM3 should be loaded normally displaying his experimental structure
	//if the user chose RM for the first time initialization will be executed automatically
	//but if the user already uses RM and he changed his options in RM1 prompt him and ask him if he wants to set a new structure
	if ((stepRM2initialized || stepRM3initialized) && RM_RMstep == 1)
	{
		//find his new stepRM1 options:
		var newRMbrepsRepInRawFiles = $('input[name=RMbreprepres]:checked').val() == "rawfiles";
		var newRMtrepsRepInRawFiles = $('input[name=RMtreprepres]:checked').val() == "rawfiles";
		var newRMconditionsRepInRawFiles = $('input[name=RMconditionrepres]:checked').val() == "rawfiles";
		if (newRMbrepsRepInRawFiles != RMbrepsRepInRawFiles || newRMtrepsRepInRawFiles != RMtrepsRepInRawFiles || newRMconditionsRepInRawFiles != RMconditionsRepInRawFiles)
		{
			if ($('input[name=RMbreprepres]:checked').val() == "rawfiles" && $('input[name=RMtreprepres]:checked').val() == "rawfiles" && $('input[name=RMconditionrepres]:checked').val() == "rawfiles")
			{
				msgbox("Something must be represented in your experiment's tags");
				RM_RMstep = 1;
				return;
			}
			questionbox("<p>You changed your options in this step. <strong>ProteoSign</strong> will clear any data pertaining to your experimental structure to setup new forms for you<br>Do you want to proceed?</p>", function(){
				//postyes function
				stepRM2initialized = false;
				stepRM3initialized = false;
				RM_RMstep++;
				showstepRMDialog();
			}, function(){
				//postno function
				if (RMbrepsRepInRawFiles)
				{
					$('input[name=RMbreprepres][value=rawfiles]').prop('checked', 'checked');
				}
				else
				{
					$('input[name=RMbreprepres][value=tags]').prop('checked', 'checked');
				}
				if (RMtrepsRepInRawFiles)
				{
					$('input[name=RMtreprepres][value=rawfiles]').prop('checked', 'checked');
				}
				else
				{
					$('input[name=RMtreprepres][value=tags]').prop('checked', 'checked');
				}
				if (RMconditionsRepInRawFiles)
				{
					$('input[name=RMconditionrepres][value=rawfiles]').prop('checked', 'checked');
				}
				else
				{
					$('input[name=RMconditionrepres][value=tags]').prop('checked', 'checked');
				}
			});
			return;
		}
	}
	RM_RMstep++;
	showstepRMDialog();
}

var onRMDialogBack_click = function()
{
	if (RM_RMstep != 1) RM_RMstep--;
	showstepRMDialog();
}

var showstepRMDialog = function()
{
	switch(RM_RMstep)
	{
		case 1:
			$("#RM1").removeClass("hidden");
			$("#RM2").addClass("hidden");
			$("#RMDialogBack").attr("disabled", true);
			$("#RMDialogNext").attr("disabled", false);
			break;
		case 2:
			if ($('input[name=RMbreprepres]:checked').val() == "rawfiles" && $('input[name=RMtreprepres]:checked').val() == "rawfiles" && $('input[name=RMconditionrepres]:checked').val() == "rawfiles")
			{
				msgbox("Something must be represented in your experiment's tags");
				RM_RMstep = 1;
				return;
			}
			$("#RMDialogBack").attr("disabled", false);
			$("#RMDialogNext").attr("disabled", true);
			$("#stepRM2txtbrep").val("");
			$("#stepRM2txttrep").val("");
			$("#stepRM2txtcondition").val("");
			RMbrepsRepInRawFiles = $('input[name=RMbreprepres]:checked').val() == "rawfiles";
			RMtrepsRepInRawFiles = $('input[name=RMtreprepres]:checked').val() == "rawfiles";
			RMconditionsRepInRawFiles = $('input[name=RMconditionrepres]:checked').val() == "rawfiles";
			//make the info in RM2 step nice:
			var infotodisplay = "";
			if (!RMbrepsRepInRawFiles && !RMtrepsRepInRawFiles && !RMconditionsRepInRawFiles)
			{
				infotodisplay = "<strong>ProteoSign</strong> considered all your raw files as fractions of the same MS/MS run, please click the Next button.";
			}
			else
			{
				infotodisplay = "Please select one or more files, define ";
				if(RMbrepsRepInRawFiles) infotodisplay += "their <i>biological replicates</i>, ";
				if(RMtrepsRepInRawFiles) infotodisplay += 'their <i>technical replicates</i> (if you do not have technical replication assign "1" to all), ';
				if(RMconditionsRepInRawFiles) infotodisplay += "their <i>conditions</i>, ";
				infotodisplay += "using the text boxes below and click <strong>Assign</strong>. The <i>fractions</i> will be assigned automatically.<br><strong>Right click</strong> on the table for extended functionality.";
			}
			//hide the tools on the bottom of the screen if nothing is represented as rawfiles
			if (!RMbrepsRepInRawFiles && !RMtrepsRepInRawFiles && !RMconditionsRepInRawFiles)
			{
				$("#stepRM2AssignmentTools").css({"display": "none"});
			}
			else
			{
				$("#stepRM2AssignmentTools").css({"display": "block"});
			}
			$("#stepRM2Info").empty();
			$("#stepRM2Info").append(infotodisplay);
			if (!RMconditionsRepInRawFiles)
			{
				RMrawfilesDT.column("4").visible(false);
				$("#stepRM2txtcondition").css({"display": "none"});
			}
			else
			{
				RMrawfilesDT.column("4").visible(true);
				$("#stepRM2txtcondition").css({"display": "block"});
			}
			if (!RMtrepsRepInRawFiles)
			{
				RMrawfilesDT.column("2").visible(false);
				$("#stepRM2txttrep").css({"display": "none"});
			}
			else
			{
				RMrawfilesDT.column("2").visible(true);
				$("#stepRM2txttrep").css({"display": "block"});
			}
			if (!RMbrepsRepInRawFiles)
			{
				RMrawfilesDT.column("1").visible(false);
				$("#stepRM2txtbrep").css({"display": "none"});
			}
			else
			{
				RMrawfilesDT.column("1").visible(true);
				$("#stepRM2txtbrep").css({"display": "block"});
			}
			RMrawfilesDT.column("3").visible(true); // fractions column
			RMrawfilesDT.clear();
			$($("#RMrawfiles").DataTable().column(0).header()).text("Raw File");
			if (!stepRM2initialized) RMrawfilesdata = [];
			var mycounter = 1;
			$.each(rawfiles, function (idx, filename_i) {
				RMrawfilesDT.row.add(
						 {
							 'fname': filename_i,
							 'brep': '-',
							 'trep': '-',
							 'frac': '-',
							 'cond': '-',
							 'DT_RowClass': "rawfiles_tbl_td_not_selected", //Notice: all rows are given the rawfiles_tbl_td_not_selected as a class whether they are selected or not, the row is selected if it has the rawfiles_tbl_td_selected class and not if it does not have it
							 'DT_RowId': 'RMtr_' + filename_i //DT_RowID is the id of the corresponding row and it is the most appendable element of the row, so in case we want to refer to the row e.g. the user has selected we will use this ID
						 }
				 );
				 //structure of RMrawfilesdata: id, name, brep, trep, frac, cond, used, selected
				 if (!stepRM2initialized) RMrawfilesdata.push([mycounter++, filename_i, '-', '-', '-', '-', 'true', false]);
			});
			$("#RM1").addClass("hidden");
			$("#RM2").removeClass("hidden");
			RMrawfilesDT.columns.adjust().draw();
			RMrawfilesDT.draw();
			//handle RMrawfiles clicks
			//the following routine is written that way so that always in RMrawfiles, the 7th element of a row (the selected one) is true if the row is selected
			lastclicked_RMrawfiles_tbl_tr = null;
			$('#RMrawfiles tbody tr').click(function (event) {
			   // console.log("			" + this);
			  if (event.shiftKey) {
				 if (lastclicked_RMrawfiles_tbl_tr !== null) {
					var i1 = $('#RMrawfiles tbody tr').index(lastclicked_RMrawfiles_tbl_tr);
					var i2 = $('#RMrawfiles tbody tr').index(this);
					var trs = $('#RMrawfiles tbody tr');
					if (i2 > i1) {
					   for (var i = (i1 + 1); i <= i2; i++) {
						  $(trs[i]).toggleClass('rawfiles_tbl_td_selected');
						  RM_set_row($(trs[i]).attr("id").substr(5));
					   }
					} else {
					   for (var i = (i1 - 1); i >= i2; i--) {
						  $(trs[i]).toggleClass('rawfiles_tbl_td_selected');
  						  RM_set_row($(trs[i]).attr("id").substr(5));

					   }
					}
				 }
			  } else {
				 $(this).toggleClass('rawfiles_tbl_td_selected');
				 RM_set_row($(this).attr("id").substr(5));
			  }
			  lastclicked_RMrawfiles_tbl_tr = this;
		   });
		   RM_refresh_fractions();
		   RM_redraw_table();
		   RM_check_next_enable();
		   stepRM2initialized = true;
		   break;
	    case 3:
			//in fact step RM3 has the same controls as step RM2 but has different rows in RMrawfiles table, notice that even though the table still has the name RMrawfiles, it now contains the tags of the experiment to assign the breps trep and conditions that remain
			//forst disable next button and empty the text boxes
			$("#RMDialogNext").attr("disabled", true);
			$("#stepRM2txtbrep").val("");
			$("#stepRM2txttrep").val("");
			$("#stepRM2txtcondition").val("");
			//make the info in RM3 step nice:
			var infotodisplay = "";
			infotodisplay = "Please select one or more tags, define ";
			if(!RMbrepsRepInRawFiles) infotodisplay += "their <i>biological replicates</i>, ";
			if(!RMtrepsRepInRawFiles) infotodisplay += 'their <i>technical replicates</i>, ';
			if(!RMconditionsRepInRawFiles) infotodisplay += "their <i>conditions</i>, ";
			infotodisplay += "using the text boxes below and click <strong>Assign</strong>.<br><strong>Right click</strong> on the table for extended functionality. When ready, click <strong>Next</strong> to submit your experimental structure.";
			$("#stepRM2Info").empty();
			$("#stepRM2Info").append(infotodisplay);
			//make sure the tools in the bottom of the screen are visible
			$("#stepRM2AssignmentTools").css({"display": "block"});
			//now make sure that the only visible elements to assign to the tags are the ones that are necessary e.g. if RMbrepsRepInRawFiles == true then the breps are represented as different raw files so we must hide the corresponding column from the user now that he assigns the breps etc. to his tags
			if (RMconditionsRepInRawFiles)
			{
				RMrawfilesDT.column("4").visible(false); // conditions column
				$("#stepRM2txtcondition").css({"display": "none"});
			}
			else
			{
				RMrawfilesDT.column("4").visible(true); // conditions column
				$("#stepRM2txtcondition").css({"display": "block"});
			}
			if (RMtrepsRepInRawFiles)
			{
				RMrawfilesDT.column("2").visible(false); // treps column
				$("#stepRM2txttrep").css({"display": "none"});
			}
			else
			{
				RMrawfilesDT.column("2").visible(true); // treps column
				$("#stepRM2txttrep").css({"display": "block"});
			}
			if (RMbrepsRepInRawFiles)
			{
				RMrawfilesDT.column("1").visible(false); // breps column
				$("#stepRM2txtbrep").css({"display": "none"});
			}
			else
			{
				RMrawfilesDT.column("1").visible(true); // breps column
				$("#stepRM2txtbrep").css({"display": "block"});
			}
			//also the fractions are not used here so hide their column
			RMrawfilesDT.column("3").visible(false); // fractions column
			$($("#RMrawfiles").DataTable().column(0).header()).text("Tag");
			RMrawfilesDT.clear();
			if (!stepRM3initialized) RMtagsdata = [];
			RMtags = peptideLabelsNamesFromFile; // get a copy of the tags of the user's dataset
			var mycounter = 1;
			$.each(RMtags, function (idx, tagname_i) {
				RMrawfilesDT.row.add(
						 {
							 'fname': tagname_i,
							 'brep': '-',
							 'trep': '-',
							 'frac': '-',
							 'cond': '-',
							 'DT_RowClass': "rawfiles_tbl_td_not_selected", //Notice: all rows are given the rawfiles_tbl_td_not_selected as a class whether they are selected or not, the row is selected if it has the rawfiles_tbl_td_selected class and not if it does not have it
							 'DT_RowId': 'RMtr_' + tagname_i //DT_RowID is the id of the corresponding row and it is the most appendable element of the row, so in case we want to refer to the row e.g. the user has selected we will use this ID
						 }
				 );
				 //structure of RMtagsdata: id, name, brep, trep, frac, cond, used, selected
				 if (!stepRM3initialized) RMtagsdata.push([mycounter++, tagname_i, '-', '-', '-', '-', 'true', false]);
			});
			RMrawfilesDT.columns.adjust().draw();
			RMrawfilesDT.draw();
			//redefine the click handler:
			lastclicked_RMrawfiles_tbl_tr = null;
			$('#RMrawfiles tbody tr').click(function (event) {
			   // console.log("			" + this);
			  if (event.shiftKey) {
				 if (lastclicked_RMrawfiles_tbl_tr !== null) {
					var i1 = $('#RMrawfiles tbody tr').index(lastclicked_RMrawfiles_tbl_tr);
					var i2 = $('#RMrawfiles tbody tr').index(this);
					var trs = $('#RMrawfiles tbody tr');
					if (i2 > i1) {
					   for (var i = (i1 + 1); i <= i2; i++) {
						  $(trs[i]).toggleClass('rawfiles_tbl_td_selected');
						  RM_set_row($(trs[i]).attr("id").substr(5));
					   }
					} else {
					   for (var i = (i1 - 1); i >= i2; i--) {
						  $(trs[i]).toggleClass('rawfiles_tbl_td_selected');
  						  RM_set_row($(trs[i]).attr("id").substr(5));

					   }
					}
				 }
			  } else {
				 $(this).toggleClass('rawfiles_tbl_td_selected');
				 RM_set_row($(this).attr("id").substr(5));
			  }
			  lastclicked_RMrawfiles_tbl_tr = this;
		   });
			//notice that now RMrawfilesDT contains the tags of the dataset
		    RM_redraw_table();
		    RM_check_next_enable();
			stepRM3initialized = true;
			break;
		case 4:
			dlgFadeoutRepMult();
			set_RMisused(true);
	}
	$(".RepMultDlg").css({"top": ($("body").height() / 2) - ($("#ReplicationMultiplexingDialog").height() / 2)});
}

var RM_set_row = function(name, state, valueidx)
{
	//taking the name of the raw file, this function finds the RMrawfilesdata record and changes the value with the index valueidx
	////structure of RMrawfilesdata: id, name, brep, trep, frac, cond, used, selected
	//e.g. to change the state of "selected" to true of raw file "WTb12" write RM_set_row("WTb12", true, 7)
	//if state is undefined then we toogle the selected value
	if (typeof(valueidx) === 'undefined') valueidx = 7;
	if (RM_RMstep == 2)
	{
		for (var i = 0 ; i <= RMrawfilesdata.length - 1; i++)
		{
			if (RMrawfilesdata[i][1] == name)
			{
				if (typeof(state) === 'undefined')
				{
					RMrawfilesdata[i][valueidx] = !RMrawfilesdata[i][valueidx];
				}
				else
				{
					RMrawfilesdata[i][valueidx] = state;
				}
			}
		}
	}
	else if (RM_RMstep == 3)
	{
		for (var i = 0 ; i <= RMtagsdata.length - 1; i++)
		{
			if (RMtagsdata[i][1] == name)
			{
				if (typeof(state) === 'undefined')
				{
					RMtagsdata[i][valueidx] = !RMtagsdata[i][valueidx];
				}
				else
				{
					RMtagsdata[i][valueidx] = state;
				}
			}
		}
	}
}

var RM_get_row = function(name, valueidx)
{
	//returns the value of the row in RMrawfilesdata of valueidx with the name name
	if (RM_RMstep == 2)
	{
		for (var i = 0 ; i <= RMrawfilesdata.length - 1; i++)
		{
			if (RMrawfilesdata[i][1] == name)
			{
				return RMrawfilesdata[i][valueidx];
			}
		}
	}
	else if (RM_RMstep == 3)
	{
		for (var i = 0 ; i <= RMtagsdata.length - 1; i++)
		{
			if (RMtagsdata[i][1] == name)
			{
				return RMtagsdata[i][valueidx];
			}
		}
	}
}
var stepRM2Assign_click = function()
{
	// the assign button in step RM2 
	// set the correct values in RMrawfilesdata table
	var items = $('#RMrawfiles').find('.rawfiles_tbl_td_selected'); //get all the selected rows
	 for (var i = 0; i < items.length; i++)
	 {
		 var items_tds = $(items[i]).find('td');
		 var items_name = items_tds[0];
		 if ($("#stepRM2txtbrep").val() != "") RM_set_row($(items_name).text(), $("#stepRM2txtbrep").val(), 2);
		 if ($("#stepRM2txttrep").val() != "") RM_set_row($(items_name).text(), $("#stepRM2txttrep").val(), 3);
		 if ($("#stepRM2txtcondition").val() != "") RM_set_row($(items_name).text(), $("#stepRM2txtcondition").val(), 5);
		 RM_set_row($(items_name).text(), 'true', 6);
	 }
	 if (RM_RMstep == 2) RM_refresh_fractions();
	 RM_redraw_table();
	 //now deselect all the rows
	 $('#RMrawfiles tbody tr').removeClass('rawfiles_tbl_td_selected');
	 if (RM_RMstep == 2)
	 {
		 for (var i = 0 ; i <= RMrawfilesdata.length - 1; i++)
		{
			RMrawfilesdata[i][7] = false;
		}
	 }
	 else if (RM_RMstep == 3)
	 {
		 for (var i = 0 ; i <= RMtagsdata.length - 1; i++)
		{
			RMtagsdata[i][7] = false;
		}
	 }
	//and clean the input text boxes
	$("#stepRM2txtbrep").val("");
	$("#stepRM2txttrep").val("");
	$("#stepRM2txtcondition").val("");
	RM_check_next_enable();
}

var RM_refresh_fractions = function()
{
	//this function autocompletes tha fractions of RMrawfiles
	$.each(RMrawfilesdata, function (idx, my_row)
	{
		if (my_row[6] == false)//if not used
		{
			return;
		}
		var my_brep = my_row[2];
		var my_trep = my_row[3];
		var my_assigned_condition = my_row[5];
		var my_cur_fraction = 1;
		if (my_brep == "-" && RMbrepsRepInRawFiles) return;
		if (my_trep == "-" && RMtrepsRepInRawFiles) return;
		if (my_assigned_condition == "-" && RMconditionsRepInRawFiles) return;
		$.each(RMrawfilesdata, function (idxJ, my_rowJ)
		{
			if (my_rowJ[6] == false) return;
			if ((my_rowJ[2] == my_brep || !RMbrepsRepInRawFiles) && (my_rowJ[3] == my_trep || !RMtrepsRepInRawFiles) && (my_rowJ[5] == my_assigned_condition || !RMconditionsRepInRawFiles))
			{
				my_rowJ[4] = my_cur_fraction++;
			}
		});
	});
}

var RM_redraw_table = function()
{
	//visualize the RMrawfilesdata array back to the user
	var items = $('#RMrawfiles').find('.rawfiles_tbl_td_not_selected,.rawfiles_tbl_td_selected'); // get all the rows
	//unfortunately the index of brep trep etc in a roware not stable and depend on step RM1
	var brepindex = 1;
	var trepindex = 2;
	var fracindex = 3;
	var condindex = 4;
	if (RM_RMstep == 3) condindex--; // if we are in RM# step the fractions column is not visible
	var lastelement;
	//in step RM3 we assign breps etc in tags so if RMbrepsRepInRawFiles == true we consider it as being false so alter these values and get them back at the end of the routine
	if (RM_RMstep == 3)
	{
		RMbrepsRepInRawFiles = !RMbrepsRepInRawFiles;
		RMtrepsRepInRawFiles = !RMtrepsRepInRawFiles;
		RMconditionsRepInRawFiles = !RMconditionsRepInRawFiles;
	}
	if (!RMbrepsRepInRawFiles)
	{
		trepindex--;
		fracindex--;
		condindex--;
	}
	if (!RMtrepsRepInRawFiles)
	{
		fracindex--;
		condindex--;
	}
	lastelement = condindex;
	if (!RMconditionsRepInRawFiles)
	{
		lastelement--;
	}
	 for (var i = 0; i < items.length; i++)
	 {
		 var items_tds = $(items[i]).find('td');
		 var items_name = items_tds[0];
		 var items_biorep = items_tds[brepindex];
         var items_techrep = items_tds[trepindex];
         var items_frac = items_tds[fracindex]; //will not be used in step RM3
         var items_condition = items_tds[condindex];
		 if (RMbrepsRepInRawFiles) $(items_biorep).text(RM_get_row($(items_name).text(), 2));
		 if (RMtrepsRepInRawFiles) $(items_techrep).text(RM_get_row($(items_name).text(), 3));
		 if (RM_RMstep == 2) $(items_frac).text(RM_get_row($(items_name).text(), 4));
		 if (RMconditionsRepInRawFiles) $(items_condition).text(RM_get_row($(items_name).text(), 5));
		 if (!(RM_get_row($(items_name).text(), 6) == 'true'))//if not used
		 {
			 for (var j = 0; j <= lastelement; j++)
			 {
				 $(items_tds[j]).css("text-decoration", "line-through");
			 }
		 }
		 else
		 {
			 for (var j = 0; j <= lastelement; j++)
			 {
				 $(items_tds[j]).css("text-decoration", "none");
			 }
		 }
	 }
	if (RM_RMstep == 3)
	{
		RMbrepsRepInRawFiles = !RMbrepsRepInRawFiles;
		RMtrepsRepInRawFiles = !RMtrepsRepInRawFiles;
		RMconditionsRepInRawFiles = !RMconditionsRepInRawFiles;
	}
}

var RM_deselect_all = function()
{
	var my_table = $('#RMrawfiles').dataTable();
	var my_rows = my_table._('tr', {"filter":"applied"});
	$.each(my_rows, function (idx, cur_row) {
		// console.log(cur_row);
		var my_tmp = cur_row["DT_RowId"];
		$(document.getElementById(my_tmp.toString())).removeClass('rawfiles_tbl_td_selected');
		RM_set_row(my_tmp.toString().substr(5), false, 7);
	});
}

var RM_check_next_enable = function()
{
	var next_should_be_enabled = true;
	if (RM_RMstep == 2)
	{
		next_should_be_enabled = is_RM_ready(2);
	}
	else if (RM_RMstep == 3)
	{
		next_should_be_enabled = is_RM_ready(3);
	}
	$("#RMDialogNext").attr('disabled', !next_should_be_enabled);
}

var is_RM_ready = function(step)
{
	// this function returns false if the RM data given by the user are not enough otherwise true
	//if step is defined it only checks the corresponding RMstep otherwise both 2 and 3
	var isready = true;
	if (typeof(step) === 'undefined' || step == 2)
	{
		for (var i = 0; i <= RMrawfilesdata.length - 1; i++)
		{
			if (!(RMrawfilesdata[i][6] == 'true')) continue; //if the raw file is not used continue
			if (RMbrepsRepInRawFiles && RMrawfilesdata[i][2] == "-")
			{
				isready = false;
				break;
			}
			if (RMtrepsRepInRawFiles && RMrawfilesdata[i][3] == "-")
			{
				isready = false;
				break;
			}
			if (RMconditionsRepInRawFiles && RMrawfilesdata[i][5] == "-")
			{
				isready = false;
				break;
			}
		}
	}
	if (typeof(step) === 'undefined' || step == 3)
	{
		for (var i = 0; i <= RMtagsdata.length - 1; i++)
		{
			if (!(RMtagsdata[i][6] == 'true')) continue; //if the raw file is not used continue
			if (!RMbrepsRepInRawFiles && RMtagsdata[i][2] == "-")
			{
				isready = false;
				break;
			}
			if (!RMtrepsRepInRawFiles && RMtagsdata[i][3] == "-")
			{
				isready = false;
				break;
			}
			if (!RMconditionsRepInRawFiles && RMtagsdata[i][5] == "-")
			{
				isready = false;
				break;
			}
		}
	}
	return isready;
}
var set_RMisused = function(isused, showmessage)
{
	if (typeof(showmessage) === 'undefined') showmessage = true;
	if (!isIsobaricLabel) return;
	//this function makes the appropriate changes in the interface in case Replication Multiplexing is decided to be used or not:
	if (isused)
	{
		//if we decided to use Rep Mult hide the main data table and display an interactive text in its place
		$("#step2ToolsWrapper").css({"display": "none"});
		$("#step2RMuseinfo").css({"display": "inline-flex"});
		$("#step2InfoHeader").css({"display": "none"});
		$("#step2desc").css({"display": "none"});
		$("#s22btnf").prop('disabled', false);
		AllowLS = false;
		AllowMergeLabels = false;
		RefreshConditionsList(RM_getRMconditions());
		$("#s3QuantitationFiltering").hide();
		$("#expquantfilt").prop("checked", false);
	}
	else
	{
		$("#step2ToolsWrapper").css({"display": "inline-flex"});
		$("#step2RMuseinfo").css({"display": "none"});
		$("#step2InfoHeader").css({"display": "block"});
		$("#step2desc").css({"display": "block"});
		$("#s3QuantitationFiltering").show();
		$("#expquantfilt").prop("checked", false);
		RM_RMstep = 1;
		RMrawfilesdata = [];
		RMtagsdata = [];
		RMbrepsRepInRawFiles = true;
		RMtrepsRepInRawFiles = true;
		RMconditionsRepInRawFiles = true;
		RMtags = [];
		RMisused = false;
		stepRM2initialized = false;
		stepRM3initialized = false;
		rawfiles_tbl_allfiles_DT.columns.adjust().draw();
		if (showmessage) msgbox("Replication Multiplexing was discarded, please define the experimental coordinates of your raw files");
		set_s22btnf();
		AllowLS = true;
		AllowMergeLabels = true;
		RefreshConditionsList(peptideLabelsNamesFromFile);
	}
	if(!AllowLS)
	{
		$("#s3showdivLabelSwapRow").hide();
	}
	else
	{
		$("#s3showdivLabelSwapRow").show();
	}
	$('#conds_list_container').contextmenu("option", "autoTrigger", AllowMergeLabels);
	RMisused = isused;
}

var onRMUndo_click = function()
{
	set_RMisused(false);
}

var onRMreview_click = function()
{
	showRepMultDialog();
}

var RM_init_RMsteps_from_load_data = function()
{
	//when RM is loaded from test data, the tables in RMsteps are in fact initialized
	stepRM2initialized = true;
	stepRM3initialized = true;
	if (RMbrepsRepInRawFiles)
	{
		$('input[name=RMbreprepres][value=rawfiles]').prop('checked', 'checked');
	}
	else
	{
		$('input[name=RMbreprepres][value=tags]').prop('checked', 'checked');
	}
	if (RMtrepsRepInRawFiles)
	{
		$('input[name=RMtreprepres][value=rawfiles]').prop('checked', 'checked');
	}
	else
	{
		$('input[name=RMtreprepres][value=tags]').prop('checked', 'checked');
	}
	if (RMconditionsRepInRawFiles)
	{
		$('input[name=RMconditionrepres][value=rawfiles]').prop('checked', 'checked');
	}
	else
	{
		$('input[name=RMconditionrepres][value=tags]').prop('checked', 'checked');
	}
}

var RM_getRMconditions = function()
{
	//this routine gets the copnditions set in Replication Multiplexing
	if (RMconditionsRepInRawFiles)
	{
		var temp_array = [];
		for(var i = 0; i<= RMrawfilesdata.length - 1; i++)
		{
			if (RMrawfilesdata[i][6] == 'true') temp_array.push(RMrawfilesdata[i][5]);
		}
		temp_array = ArrNoDupe(temp_array);
	}
	else
	{
		var temp_array = [];
		for(var i = 0; i<= RMtagsdata.length - 1; i++)
		{
			if (RMtagsdata[i][6] == 'true') temp_array.push(RMtagsdata[i][5]);
		}
		temp_array = ArrNoDupe(temp_array);
	}
	return temp_array;
}

//the following routine inserts conditions in conditions_list
var RefreshConditionsList = function(my_conditions)
{
	$("#conditions_list").empty();
	$.each(my_conditions, function (idx, lblname_i) {
		$("#conditions_list").append("<option value='" + lblname_i + "' style='padding: 2px; font-size: 125%;' selected='true'>" + lblname_i + "</option>");
    });
}
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
	   if ($(btn).attr("id") == "s22btnb")
	   {
		   $(btn).on("click", function () {
			   carefulBackStep = 2;
			   CarefulBack();
		   });
	   }
	   else if($(btn).attr("id") == "s4btnb")
	   {
		   $(btn).on("click", function () {
			   carefulBackStep = 4;
			   CarefulBack();
		   });
	   }
	   else
	   {
		  $(btn).on("click", function () {
			 toggleNextClass(idsToStrings(".main_div .main_section").reverse(), "hidden", true, rollbackStage);
			 rawfiles_tbl_allfiles_DT.columns.adjust().draw();
		  });
	   }
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
						msgbox("WARNING: Some of the files you uploaded are processed by MaxQuant and some by Proteome Discoverer, we are sorry to inform you that it is impossible to proceed.");
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
					msgbox("WARNING: You uploaded too many files from " + procProgram + "! Please make sure you upload MultiConsensus files. Please refresh the current page and try again.");
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
				msgbox("WARNING: At least one of the files chosen is oversized, we are sorry to inform you that it is impossible to proceed.");
	         $("#s2btnf").prop('disabled', true);
			}
      } else {
         //$("#s2btnf").prop('disabled', true);
      }
		$("#__s2btnupld").val("");
   });
   
   $("#__upldparams").change(function () {
		if ($("#__upldparams").val() == "") {
			return;
		}
		var fnames = "";
		if (this.files.length > 1) return;
		if (this.files.length > 0) {
			uploadingFile = this.files[0];
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
				 var thedata = new FormData();
				   thedata.append('thefile', uploadingFile);
				   thedata.append('session_id', sessionid);
				   $.ajax({
					  url: cgi_bin_path + 'upload_param_file.php', //Server script to receive file
					  type: 'POST',
					        // Form data
						  data: thedata,
						  //Options to tell jQuery not to process data or worry about content-type.
						  cache: false,
						  contentType: false,
						  processData: false,
				   }).done(function (data, textStatus, jqXHR) {
					   LoadParams(data.restext);
					   $("#__upldparams").val("");
				   });
			}
		}
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
    $("#s3showdivAdvancedOptions").on("click", function () {
	   	$(".expparamsDlg").css({"left" : ($("body").width()/2) - ($("#divAdvancedOptions").width()/2)});
		$('body').append('<div id="mask"></div>');
		$("#divAdvancedOptions").fadeIn(300);
		$('#mask').fadeIn(300);
		$("#ADVoptionsOK").on("click", function () {
			 
			 //ADD OK RESULT HERE
		});
		InitializeAdvParam();
   }); 
    $("#s2linkSaveParams").on("click", function () {
		SaveParams(0);
   });

   $("#s3linkSaveParams").on("click", function () {
		SaveParams(0);
   }); 
   $("#s4linkSaveParams").on("click", function () {
		SaveParams(0);
   }); 
    $("#s2linkLoadParams").on("click", function () {
		getparamfile();
		my_step = 2;
   }); 
   $("#s3linkLoadParams").on("click", function () {
		getparamfile();
		my_step = 3;
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
			$(items_tds[0]).css("text-decoration", "none");
			$(items_tds[1]).css("text-decoration", "none");
			$(items_tds[2]).css("text-decoration", "none");
			$(items_tds[3]).css("text-decoration", "none");
			if (isLabelFree == true) $(items_tds[4]).css("text-decoration", "none");
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
   for (var i = 1; i <= 5; i++)
   {
	   var myid = "#star" + i;
	   $(myid).mouseleave(function(){
		   if (!stars_enabled) return;
		   star_hovered = star_chosen;
		   display_star_score();
	   });
	   $(myid).click(function(){
		   if (!stars_enabled) return;
		   star_chosen = star_hovered;
		   display_star_score();
	   });
   }
   $("#star1").mouseenter(function(){
	   star_hovered = 1;
	   display_star_score();
   });  
   $("#star2").mouseenter(function(){
	   star_hovered = 2;
	   display_star_score();
   });
   $("#star3").mouseenter(function(){
	   star_hovered = 3;
	   display_star_score();
   });
   $("#star4").mouseenter(function(){
	   star_hovered = 4;
	   display_star_score();
   });
   $("#star5").mouseenter(function(){
	   star_hovered = 5;
	   display_star_score();
   });
   if (!stars_enabled)
   {
	   $("#Feedbackstars").css("display", "none");
   }
   // Bind test datasets dialog buttons
   $("#dlgTestDatasetsBtnOK").on("click", function () {
	  dlgFadeout();
	  resetState(); //start a new session to make sure the only files uploaded will be the ones from the test dataset
	  //also empty s2uluploaders to show the user that the only files used will be the ones of the test dataset:
	  $("s2uluploaders").empty();
	  $("#s2btnupld").prop('disabled', true);
	  //log in php that the current dataset is a test one
	  log_test_dataset = true;
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
   // Initialize the rawfiles table in the Replication Multiplexing dialog (step RM2)
   RMrawfilesDT = $("#RMrawfiles").DataTable({
	   paging: false,
      bInfo: false,
	  select: true,
	  "pageLength": 308,
	  scrollY: "200px",
	  scrollCollapse: true,
	     "columnDefs": [
         {
            targets: [1, 2, 3, 4],
            width: '4%',
            className: "dt-center"
         },
         {
            targets: 0,
            width: '100%',
            className: "dt-left",
			title: "Raw File"
         },
		 {
			 targets: 1,
			title: "#B",
		 },
		 {
			 targets: 2,
			title: "#T",
		 },
		 {
			 targets: 3,
			title: "#F",
		 },
		 {
			 targets: 4,
			 title: "Condition",
			 width: '18%',
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
		delegate: ".rawfiles_tbl td",
		menu: [
		  {title: "Select All", cmd: "slc_all"},
		  {title: "Deselect All", cmd: "dslc_all"},
		  {title: "Invert Selection", cmd: "inv_slc"},
		  {title: "Exclude Selected", cmd: "rmv_slc"},
		  {title: "Include Selected", cmd: "add_slc"},
		  {title: "Clear filter", cmd: "clr_fltr"},
		  {title: "Reset", cmd: "reset"},
		  {title: "Select unassigned", cmd: "slc_unassigned"},
		  {title: "Assign Condition", cmd: "assign_condition", visible: false},
		  {title: "Replication Multiplexing", cmd: "rep_mult", visible: false}
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
							if(my_raw_file.rawfile == $(items_tds[0])[0].textContent)
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
						$(items_tds[0]).css("text-decoration", "line-through");
						$(items_tds[1]).css("text-decoration", "line-through");
						$(items_tds[2]).css("text-decoration", "line-through");
						$(items_tds[3]).css("text-decoration", "line-through");
						if (isLabelFree == true) $(items_tds[4]).css("text-decoration", "line-through");
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
							if(my_raw_file.rawfile == $(items_tds[0])[0].textContent)
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
						$(items_tds[0]).css("text-decoration", "none");
						$(items_tds[1]).css("text-decoration", "none");
						$(items_tds[2]).css("text-decoration", "none");
						$(items_tds[3]).css("text-decoration", "none");
						if (isLabelFree == true) $(items_tds[4]).css("text-decoration", "none");
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
				case "rep_mult":
					showRepMultDialog();
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
							 Refresh_conds_list_cmenu_items();
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
						 Refresh_conds_list_cmenu_items();
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
		//the following code handles the context menu in Replication Multiplexing
		RMcontext_menu = $("#RMtablecontainer").contextmenu({
			delegate: ".RMrawfilestbl td",
			menu: [
			  {title: "Select All", cmd: "slc_all"},
			  {title: "Deselect All", cmd: "dslc_all"},
			  {title: "Invert Selection", cmd: "inv_slc"},
			  {title: "Exclude Selected", cmd: "rmv_slc"},
			  {title: "Include Selected", cmd: "add_slc"},
			  {title: "Clear filter", cmd: "clr_fltr"},
			  {title: "Reset", cmd: "reset"},
			  {title: "Select unassigned", cmd: "slc_unassigned"}
			],
			select: function(event, ui) {
				switch(ui.cmd){
					case "slc_all":
						var my_table = $('#RMrawfiles').dataTable();
						var my_rows = my_table._('tr', {"filter":"applied"});
						$.each(my_rows, function (idx, cur_row) {
							// console.log(cur_row);
							var my_tmp = cur_row["DT_RowId"];
							$(document.getElementById(my_tmp.toString())).addClass('rawfiles_tbl_td_selected');
							RM_set_row(my_tmp.toString().substr(5), true, 7);
							});
						break;
					case "reset":
						if (RM_RMstep == 2)
						{
							RMrawfilesdata = [];
							var mycounter = 1;
							$.each(rawfiles, function (idx, filename_i) {
								RMrawfilesdata.push([mycounter++, filename_i, '-', '-', '-', '-', true, false]);
							});
							RM_refresh_fractions();
						}
						else if (RM_RMstep == 3)
						{
							RMtagsdata = [];
							RMtags = peptideLabelsNamesFromFile;
							var mycounter = 1;
							$.each(RMtags, function (idx, tag_i) {
								RMtagsdata.push([mycounter++, tag_i, '-', '-', '-', '-', true, false]);
							});
						}
						RM_redraw_table();
						//notice the absence of break, reseting the state also deselects all
					case "dslc_all":
						RM_deselect_all();
						break;
					case "inv_slc":
						var my_table = $('#RMrawfiles').dataTable();
						var my_rows = my_table._('tr', {"filter":"applied"});
						$.each(my_rows, function (idx, cur_row) {
							// console.log(cur_row);
							var my_tmp = cur_row["DT_RowId"];
							$(document.getElementById(my_tmp.toString())).toggleClass('rawfiles_tbl_td_selected');
							RM_set_row(my_tmp.toString().substr(5));
							});	
						break;
					case "clr_fltr":
						RMrawfilesDT.search('');
						RMrawfilesDT.columns().search('');
						RMrawfilesDT.draw();
						break;
					case "rmv_slc":
						var my_table = $('#RMrawfiles').dataTable();
						var items = $('#RMrawfiles').find('.rawfiles_tbl_td_selected');
						for (var i = 0; i < items.length; i++) {
							var items_tds = $(items[i]).find('td');
							RM_set_row($(items_tds[0]).text(), 'false', 6);//set the row to unused
						}
						if (RM_RMstep == 2) RM_refresh_fractions();
						RM_redraw_table();
						RM_deselect_all();
						break;
					case "add_slc":
						var my_table = $('#RMrawfiles').dataTable();
						var items = $('#RMrawfiles').find('.rawfiles_tbl_td_selected');
						for (var i = 0; i < items.length; i++) {
							var items_tds = $(items[i]).find('td');
							RM_set_row($(items_tds[0]).text(), 'true', 6);//set the row to unused
						}
						if (RM_RMstep == 2) RM_refresh_fractions();
						RM_redraw_table();
						RM_deselect_all();
						break;
					case "slc_unassigned":
						var my_table = $('#RMrawfiles').dataTable();
						var my_rows = my_table._('tr', {"filter":"applied"});
						$.each(my_rows, function (idx, cur_row) {
							var mustbeselected = false;
							if (RM_RMstep == 3)
							{
								RMbrepsRepInRawFiles = !RMbrepsRepInRawFiles;
								RMtrepsRepInRawFiles = !RMtrepsRepInRawFiles;
								RMconditionsRepInRawFiles = !RMconditionsRepInRawFiles;
							}
							if (RMbrepsRepInRawFiles && RM_get_row(cur_row["DT_RowId"].toString().substr(5), 2) == "-") mustbeselected = true;
							if (RMtrepsRepInRawFiles && RM_get_row(cur_row["DT_RowId"].toString().substr(5), 3) == "-") mustbeselected = true;
							if (RMconditionsRepInRawFiles && RM_get_row(cur_row["DT_RowId"].toString().substr(5), 5) == "-") mustbeselected = true;
							if (RM_RMstep == 3)
							{
								RMbrepsRepInRawFiles = !RMbrepsRepInRawFiles;
								RMtrepsRepInRawFiles = !RMtrepsRepInRawFiles;
								RMconditionsRepInRawFiles = !RMconditionsRepInRawFiles;
							}
							if (RM_get_row(cur_row["DT_RowId"].toString().substr(5), 6) == 'false') mustbeselected = false;
							if (mustbeselected)
							{
								$(document.getElementById(cur_row["DT_RowId"].toString())).addClass('rawfiles_tbl_td_selected');
								RM_set_row(cur_row["DT_RowId"].toString().substr(5), true, 7);
							}
							else
							{
								$(document.getElementById(cur_row["DT_RowId"].toString())).removeClass('rawfiles_tbl_td_selected');
								RM_set_row(cur_row["DT_RowId"].toString().substr(5), false, 7);
							}
						});
						break;
				}
			RM_check_next_enable(); //check if the next button in RM Dialog must be enabled
			},
			beforeOpen: function(event, ui) {
			var $menu = ui.menu,
				$target = ui.target,
				extraData = ui.extraData;
			ui.menu.zIndex(99999);
			}
		});
   $('#rawfiles_tbl_allfiles').css({border: 'none'});
   $('#rawfiles_tbl_allfiles').css({margin: '0px'});
   $('#rawfiles_tbl_allfiles thead th').css({border: 'none'});
   $('#rawfiles_tbl_allfiles').css({'border-bottom': 'none'});
   $('#rawfiles_tbl_allfiles').css({'border-bottom': 'none'});
   //obsolete:
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
   //end obsolete
   
	//from http://stackoverflow.com/questions/8641729/how-to-avoid-the-need-for-ctrl-click-in-a-multi-select-box-using-javascript
	$("#conditions_list").mousedown(function(e){
		e.preventDefault();
		if (e.which == 3)
		{
			return;
		}
		var select = this;
		var scroll = select .scrollTop;

		e.target.selected = !e.target.selected;

		setTimeout(function(){select.scrollTop = scroll;}, 0);

		$(select).focus();
	}).mousemove(function(e){e.preventDefault()});
	
	$("#userFeedback").on('change keyup paste', function() {
		onuserFeedbackchange();
	});

	
   // TEST DATA INIT
   postTestDatasetsInfo();
   //
   postClientServerClientInfo();
});