var sessionid = new Date().getTime();
var clientname = '';
var softversion = '';
var cgi_bin_path = 'cgi-bin/';

// from: http://www.shamasis.net/2009/09/fast-algorithm-to-find-unique-items-in-javascript-array/
Array.prototype.unique = function () {
   var o = {}, i, l = this.length, r = [];
   for (i = 0; i < l; i += 1)
      o[this[i]] = this[i];
   for (i in o)
      r.push(o[i]);
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

//modified from http://stackoverflow.com/questions/17964108/select-multiple-html-table-rows-with-ctrlclick-and-shiftclick
var lastSelectedRow;
var trs;

function RowClick(e) {
   var event = e || window.event;
   var currenttr = (event.target ? event.target : event.srcElement);
   var lock = false;
   if (event.ctrlKey) {
      toggleRow(currenttr);
   }
   if (event.button === 0) {
      if (!event.ctrlKey && !event.shiftKey) {
         clearAll();
         toggleRow(currenttr);
      }

      if (event.shiftKey) {
         selectRowsBetweenIndexes([lastSelectedRow.parentNode.rowIndex, currenttr.parentNode.rowIndex])
      }
   }
}

function toggleRow(row) {
   row.className = row.className == 'rawfiles_tbl_td_selected' ? 'rawfiles_tbl_td_not_selected' : 'rawfiles_tbl_td_selected';
   lastSelectedRow = row;
}

function selectRowsBetweenIndexes(indexes) {
   indexes.sort(function (a, b) {
      return a - b;
   });

   for (var i = indexes[0]; i < indexes[1]; i++) {
      trs[i].className = 'rawfiles_tbl_td_selected';
   }
}

function clearAll() {
   for (var i = 0; i < trs.length; i++) {
      trs[i].className = 'rawfiles_tbl_td_not_selected';
   }
}

var inputChCheck = function (e, repatt, maxCharacters) {
   var theEvent = e || window.event;
   var key = theEvent.keyCode || theEvent.which;
   if (key === 8) {
      return;
   }
   var re = new RegExp(repatt);
   var srcelem = e.target || e.srcElement;
   var txt = srcelem.value + String.fromCharCode(e.which);
   if (!re.test(txt) || txt.length > maxCharacters) {
      e.preventDefault();
   }
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
   $(selector).attr(json);
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
      var match = ($(element).attr("id")).match(id_pattern);
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
      alert(cssstr);
      $(selector).css(attr, cssstr.replace(/rgb\([0-9]+, [0-9]+, [0-9]+\)$/g, hexcolor));
   }
}

// Clear areas where results information appears.
var resetResultsInfoItms = function () {
   $("#server_feedback").empty();
   $("#dndres").empty();
   $("#results_p").html("Plase wait (up to 5 minutes) for your results.");
}

var postFireUpAnalysisAndWait = function () {
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
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
      success: function (data, textStatus, jqXHR) {
         $("#server_feedback").empty();
         $("#s4btnf").prop('disabled', !data.success);
         if (data.success) {
            $("#results_p").html("Now you can inspect your results. When ready, click <em>Next</em>.");
            $("#dndres").html("<span><a href=" + data.results_url + "><strong>" + data.results_url.substr(data.results_url.lastIndexOf("/") + 1) + "</strong></a></span>");
            patt = new RegExp("limma\-graphs");
            $.each(data.results_preview, function (idx, path_to_img_i)
            {
               var img_i = path_to_img_i.substr(data.results_url.lastIndexOf("/") + 1);
               if (!patt.test(img_i)) {
                  $("#server_feedback").append("<div class='resimg'><a href='" + path_to_img_i + "' target='_blank'><img src='" + path_to_img_i + "' width='120'></img></a></div>");
               }
            });
         } else {
            $("#results_p").html("");
            $("#server_feedback").html("<span class='uploadErrorMsg'><strong><em>The analysis could not be completed: " + data.msg + "<em><strong></span>");
            if (data.R_dump.length > 0) {
               $("#server_feedback").append("<br><br><span style='font-family: Georgia; font-size: 95%; text-align: left; display: inline-block; width: 90%'><p>Please ensure that input parameters (such as number of replicates, number of biological conditions/labels etc) are correctly defined and input data format is valid. The statistical analysis routine relies heavily on the validity of the input parameters, i.e. minor deviations from the correct parameter values may bring the analysis to a halt.</p><p>If the above does not apply, then the statistical analysis may have failed due to numerical problems (e.g. there were too many missing values/data points).</p><p> If you feel that none of the above is the case, please click <a href='mailto:msdiffexp@gmail.com?Subject=Session%20" + sessionid + "' target='_blank'><u>here</u></a> to notify via e-mail (do not delete the session id in the subject) the ProteoSign team for investigation of your analysis issue.</p></span>")
               //$("#server_feedback").append("<br><br><span style='font-family: \"Courier New\"'>Logged information:</span><br>");
               //$("#server_feedback").append("<div style='height: inherit'><textarea style='font-family: \"Courier New\"; font-size: 90%; width: 90%; height: 100%' readonly>"+ data.dump +"</textarea></div>");
            }
         }
      },
      error: function (jqXHR, textStatus, errorThrown) {
         $("#server_feedback").empty();
         $("#server_feedback").html("<span class='uploadErrorMsg'><strong><em>An AJAX error occurred: " + errorThrown + "<em><strong></span>");
      }
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
      },
      success: function (data, textStatus, jqXHR) {
         clientname = data.hostname;
         softversion = data.version;
         $("#scrollingtext").html("Welcome <em>" + clientname + "</em> to ProteoSign");
         $("#proteosignversion").html("ProteoSign version " + softversion);
      },
      error: function (jqXHR, textStatus, errorThrown) {
      }
   });
}

var postParameters = function (params) {
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   $.each(params, function (idx, param_i)
   {
      switch ($(param_i).attr('type')) {
         case "checkbox":
            // Here the on/off is transmitted Yes/No
            var theval = ($(param_i).prop("checked") ? "Yes" : "No");
            thedata.append($(param_i).attr('name'), theval);
            break;
         default:
            var theval = $(param_i).val();
            if (theval == null) {
               theval = '';
            }
            thedata.append($(param_i).attr('name'), theval);
            break;
      }
      //console.log($(param_i).attr('name')+" = "+theval);
   });
   thedata.append("labelfree", ((peptideLabelsNamesFromFile.length == 0 && peptideLabelsFromFile.length > 0) ? 'Yes' : 'No'));
   thedata.append("exp_struct", gen_expdesign(rawfiles_structure));
   $.ajax({
      url: cgi_bin_path + 'upload_parameters.php', //Server script to receive parameters
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false,
      success: function (data, textStatus, jqXHR) {
         //if there was a server-side error alert.
         if (!data.success) {
            alert("ERROR on SERVER: " + data.msg);
         } else {
            postFireUpAnalysisAndWait();
         }
      }
   });
}

// Executed when a "Next" type of button is clicked
var executeStage = function (stageIndex) {
   var ret = true;
   switch (stageIndex) {
      case 1:
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
         break;
      default:
   }
   return ret;
}
// Executed when a "Back" type of button is clicked
var rollbackStage = function (stageIndex) {
   var ret = true;
   return ret;
}

var unsuccessfullyUploadedFiles = {};
var uploadEfforts = {};
var peptideLabelsFromFile = [];
var peptideLabelsNamesFromFile = [];
var peptideLabelsFromFileCombs = [];
var nFormLabels = 1;
var nToUpload;
var debug_ajax_data;

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
   return nInvalid == 0;
}

// reset "counters"/states
var resetState = function () {
   unsuccessfullyUploadedFiles = {};
   uploadEfforts = {};
   peptideLabelsFromFile = [];
   peptideLabelsNamesFromFile = [];
   peptideLabelsFromFileCombs = [];
   rawfiles = undefined;
   // Reset items that contained information from previous data input files (e.g. label information)
   $("#s3expparamsDlgLabelsSelection").empty();
   $("#explbl1name_").empty();
   $("#explbl0name_").empty();
   $("#s3advparams select[name='expquantfiltlbl']").empty();
   $("#quantsoftdetectedspan").empty();
   if (!$("#explbl1definition").hasClass("hidden")) {
      $("#explbl1definition").addClass("hidden");
   }
   //remove progress bar(s)
   $("#s2uluploaders > table").empty();
   nUploaded = 0;
   sessionid = new Date().getTime();
}

// "uploadingFiles": Files (array returned by FileChooser) selected for upload in stage #2
var uploadFiles = function (uploadingFiles, serverSide, postSuccess) {
   resetState();
   nToUpload = uploadingFiles.length;
   $("#s2btnf").prop('disabled', true);
   $.each(uploadingFiles, function (idx, file_i) {
      $("#s2uluploaders table").append("<tr><td>" + file_i.toString() + "</td><td><progress max='100' value='0' id=uploadfile" + idx + "><div class='progress-bar'><span style='width: 80%;'></span></div></progress></td></tr>");
   });
   // fire AJAX calls
   $.each(uploadingFiles, function (idx, file_i)
   {
      postFile(idx, file_i, serverSide, postSuccess);
   });
}

// Called by respective AJAX event handlers (see postFile function below)
var uploadFinished = function (success, idx, file) {
   uploadEfforts[idx] = file;
   if (!success) {
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
                  if(e.loaded / e.total == 1.0){
                     $(progresstditm).html("<span class='uploadSuccessMsg'><strong><em>Processing ...<em><strong></span>");
                  }else{
                     helper_setItemAttr("#uploadfile" + idx, {value: e.loaded, max: e.total});
                  }
               }
            }, false); // For handling the progress of the upload
         }
         return myXhr;
      },
      //Ajax events
      beforeSend: function (jqXHR, settings) {
         if (serverSide) {
            helper_setItemAttr("#uploadfile" + idx, {value: 0, max: 100});
         }
      },
      success: function (data, textStatus, jqXHR) {
         debug_ajax_data = data;
         //If server-side everything went fine (internal things that the server had to do with the client's file, such as storage etc)
         uploadFinished(data.success, idx, file);
         //if everything went fine enable button for next stage and print OK. Just print the error message otherwise.
         if (data.success) {
            $(progresstditm).html("<span class='uploadSuccessMsg'><strong><em>OK<em><strong></span>");
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
                  $(item5).removeClass('hidden');
                  $(item6).attr('placeholder', 'Definition');
               } else {
                  //label-free case (disable non-applicable parameters and rename others accordingly)
                  $(item1).prop('disabled', true);
                  $(item2).prop('disabled', true);
                  $(item3).html('&#8212 Biological condition #1');
                  $(item4).html('<input data-required="true" id="explbl1name_" name="explbl1name" type="text" onkeypress="inputChCheck(event,\'^(?!_)[a-zA-Z0-9_]+$\',20)" placeholder="Character rules apply"></input>');
                  $(item5).addClass('hidden');
                  $(item6).attr('placeholder', 'Raw file');
               }

               $.each(peptideLabelsFromFile, function (idx, lbl_i) {
                  $("#s3expparamsDlgLabelsSelection").append("<option value='" + lbl_i + "'>" + lbl_i + "</option>");
               });
               $.each(peptideLabelsNamesFromFile, function (idx, lblname_i) {
                  $("#explbl1name_").append("<option value='" + lblname_i + "'>" + lblname_i + "</option>");
                  $("#explbl0name_").append("<option value='" + lblname_i + "'>" + lblname_i + "</option>");
                  $("#s3advparams select[name='expquantfiltlbl']").append("<option value='" + lblname_i + "'>" + lblname_i + "</option>");
               });
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
      },
      error: function (jqXHR, textStatus, errorThrown) {
         $(progresstditm).empty();
         $(progresstditm).html("<span class='uploadErrorMsg'><strong><em>An AJAX error occurred: " + errorThrown + "<em><strong></span>");
         uploadFinished(false, idx, file);
      },
      // Form data
      data: thedata,
      //Options to tell jQuery not to process data or worry about content-type.
      cache: false,
      contentType: false,
      processData: false
   });
}

var bind_explbldefinition_focus = function (explbldefinition) {
   $(explbldefinition).on("focus", function () {
      $(".expparamsDlg").css({"left": ($("body").width() / 2) - ($("#s3expparamsDlgLabels").width() / 2)});
      $('body').append('<div id="mask"></div>');
      $("#s3expparamsDlgLabels").fadeIn(300);
      $('#mask').fadeIn(300);
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

var onChooseFromTestDatasets = function () {
   $(".expparamsDlg").css({"left": ($("body").width() / 2) - ($("#s1TestDatasets").width() / 2)});
   $('body').append('<div id="mask"></div>');
   $("#s1TestDatasets").fadeIn(300);
   $('#mask').fadeIn(300);
}

var addFormLabel = function () {
   if (!$("#explbl" + nFormLabels + "definition").hasClass("hidden") && $("#explbl" + nFormLabels + "definition").val().length == 0) {
      return;
   }
   nFormLabels++;
   var last_tr_of_table = $("#btnAddLabel").closest("table").children().first().children().last();
   if (peptideLabelsNamesFromFile.length > 0) {
      $(last_tr_of_table).after("<tr><td>&#8212 Species label #" + nFormLabels + "</td><td></td><td><select data-required='true' id='explbl" + nFormLabels + "name_' name='explbl" + nFormLabels + "name'></select></td><td><input data-required='true' id='explbl" + nFormLabels + "definition' name='explbl" + nFormLabels + "def' type='text' placeholder='Definition' readonly" + (peptideLabelsFromFile.length > 0 ? "" : " class='hidden'") + "/></td></tr>");
      if (peptideLabelsFromFile.length > 0) {
         bind_explbldefinition_focus("#explbl" + nFormLabels + "definition");
         $("#explbl" + (nFormLabels - 1) + "definition").off("focus");
      }
      $.each(peptideLabelsNamesFromFile, function (idx, lblname_i) {
         $("#explbl" + nFormLabels + "name_").append("<option value='" + lblname_i + "'>" + lblname_i + "</option>");
      });
   } else {
      //label-free case
      if (peptideLabelsFromFile.length > 0) {
         $(last_tr_of_table).after("<tr><td>&#8212 Biological condition #" + nFormLabels + "</td><td></td><td><input data-required='true' id='explbl" + nFormLabels + "name_' name='explbl" + nFormLabels + "name' type='text' onkeypress='inputChCheck(event,\"^(?!_)[a-zA-Z0-9_]+$\",20)' placeholder='Character rules apply'></input></td><td><input data-required='true' id='explbl" + nFormLabels + "definition' name='explbl" + nFormLabels + "def' type='text' placeholder='Raw file' readonly" + (peptideLabelsFromFile.length > 0 ? "" : " class='hidden'") + "/></td></tr>");
         bind_explbldefinition_focus("#explbl" + nFormLabels + "definition");
         $("#explbl" + (nFormLabels - 1) + "definition").off("focus");
      }
   }
   var addbtntd = $("#btnAddLabel").closest("td");
   var rembtntd = $(addbtntd).next();
   last_tr_of_table = $(last_tr_of_table).next();
   $(last_tr_of_table).children().last().after($(addbtntd));
   $(last_tr_of_table).children().last().after($(rembtntd));
   $("#btnRemLabel").prop("disabled", false);
   // label-free case
   if (peptideLabelsNamesFromFile.length == 0 && peptideLabelsFromFile.length > 0) {
      $("#btnAddLabel").prop("disabled", nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsFromFile.length - 2) : peptideLabelsFromFile.length - 1));
   } else {
      $("#btnAddLabel").prop("disabled", nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsNamesFromFile.length - 2) : peptideLabelsNamesFromFile.length - 1));
   }
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
      contentType: false,
      success: function (data, textStatus, jqXHR) {
         //if there was a server-side error alert.
         if (!data.success) {
            alert("ERROR on SERVER: " + data.msg);
         } else {
            uploadingFiles = data.queryres.file;
            if (data.queryres.file.length > 0) {
               //Start uploading ...
               uploadFiles(data.queryres.file, true, function () {
                  if (++nUploaded < uploadingFiles.length) {
                     return;
                  }
                  $("#s2btnf").triggerHandler("click");
                  $("#s3showhideadvparams").trigger("click");
                  $("input[name='expid']").val(dataset_desc.replace(/[^a-zA-Z0-9]+/g, "_"));
                  $.each(data.queryres.selector, function (idx, param_selector)
                  {
                     //console.log(param_selector + " = " + data.queryres.value[idx]);
                     switch ($(param_selector).attr('type')) {
                        case "checkbox":
                           // Here the 0/1 is transmitted false/true
                           var theval = (data.queryres.value[idx] == "0" ? false : true);
                           if ($(param_selector).prop("checked") != theval) {
                              $(param_selector).trigger("click");
                           }
                           break;
                        default:
                           m = param_selector.match(/^#explbl[1-9]+[0-9]*name_$/);
                           if (m != null && !$(param_selector).is(':visible')) {
                              addFormLabel();
                           }
                           $(param_selector).val(data.queryres.value[idx]);
                           break;
                     }
                  });
                  // simulate user-based experimental structure building via the GUI
                  var i;
                  var tmp_map = [];
                  for (i = 0; i < trs.length; i++) {
                     tmp_map[trs[i].childNodes[0].nodeValue] = i;
                  }
                  var n_rawfiles = data.queryres.raw_file.length;
                  var n_breps = data.queryres.brep.unique().length;
                  var n_treps = data.queryres.trep.unique().length;
                  var n_frac = data.queryres.frac.unique().length;
                  if (n_treps > 1) {
                     if (n_frac > 1) {
                        // raw files correpsond to fractions
                        for (var brep_i = 1; brep_i <= n_breps; brep_i++) {
                           for (var trep_i = 1; trep_i <= n_treps; trep_i++) {
                              //select multiple trs
                              clearAll();
                              for (var j = 0; j < data.queryres.raw_file.length; j++) {
                                 if (data.queryres.brep[j] == brep_i && data.queryres.trep[j] == trep_i) {
                                    trs[tmp_map[data.queryres.raw_file[j]]].className = 'rawfiles_tbl_td_selected';
                                 }
                              }
                              //define coords
                              $('#expstructcoord_biorep').val(brep_i);
                              $('#expstructcoord_techrep').val(trep_i);
                              //trigger
                              $('#btnAssignExpStructCoord').click();
                           }
                        }
                     } else {
                        // raw files correpsond to technical replicates
                        for (var brep_i = 1; brep_i <= n_breps; brep_i++) {
                           //select multiple trs
                           clearAll();
                           for (var j = 0; j < data.queryres.raw_file.length; j++) {
                              if (data.queryres.brep[j] == brep_i) {
                                 trs[tmp_map[data.queryres.raw_file[j]]].className = 'rawfiles_tbl_td_selected';
                              }
                           }
                           //define coords
                           $('#expstructcoord_biorep').val(brep_i);
                           $('#expstructcoord_techrep').val('');
                           //trigger
                           $('#btnAssignExpStructCoord').click();
                        }
                     }
                  } else {
                     if (n_frac > 1) {
                        // raw files correpsond to fractions
                        for (var brep_i = 1; brep_i <= n_breps; brep_i++) {
                           for (var trep_i = 1; trep_i <= n_treps; trep_i++) {
                              //select multiple trs
                              clearAll();
                              for (var j = 0; j < data.queryres.raw_file.length; j++) {
                                 if (data.queryres.brep[j] == brep_i && data.queryres.trep[j] == trep_i) {
                                    trs[tmp_map[data.queryres.raw_file[j]]].className = 'rawfiles_tbl_td_selected';
                                 }
                              }
                              //define coords
                              $('#expstructcoord_biorep').val(brep_i);
                              $('#expstructcoord_techrep').val(trep_i);
                              //trigger
                              $('#btnAssignExpStructCoord').click();
                           }
                        }
                     } else {
                        // raw files correpsond to breps
                        for (var brep_i = 1; brep_i <= n_breps; brep_i++) {
                           //select tr
                           clearAll();
                           trs[tmp_map[data.queryres.raw_file[data.queryres.brep.indexOf(brep_i)]]].className = 'rawfiles_tbl_td_selected';
                           //define coords
                           $('#expstructcoord_biorep').val(brep_i);
                           $('#expstructcoord_techrep').val('');
                           //trigger
                           $('#btnAssignExpStructCoord').click();
                        }
                     }
                  }
                  clearAll();
                  $('#expstructcoord_biorep').val('');
                  $('#expstructcoord_techrep').val('');
                  //
                  $("#s3expparams").animate({scrollTop: 0}, "slow");
               });
            } else {
               $("#s2btnf").prop('disabled', true);
            }
         }
      },
      error: function (jqXHR, textStatus, errorThrown) {
         alert("An AJAX error occurred: " + errorThrown);
      }
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
      contentType: false,
      success: function (data, textStatus, jqXHR) {
         //if there was a server-side error alert.
         if (!data.success) {
            alert("ERROR on SERVER: " + data.msg);
         } else {
            var i = 1;
            $.each(data.queryres.desc, function (idx, dataset_desc)
            {
               $("#s1TestDatasetsSelection").append("<option value='" + (i++) + "'>" + dataset_desc + "</option>");
            });
         }
      },
      error: function (jqXHR, textStatus, errorThrown) {
         alert("An AJAX error occurred: " + errorThrown);
      }
   });
}

var bioreps;
var techreps;
var fractions;
var rawfiles;
var rawfiles_structure;
var rep_counts;

var reset_reps = function () {
   bioreps = 0;
   techreps = 0;
   fractions = 0;
   rawfiles_structure = [];
   rep_counts = {biorep: []};
   $('#rawfiles_tbl_allfiles td').parent().remove();
   $.each(rawfiles, function (idx, filename_i) {
      $('#rawfiles_tbl_allfiles > tbody:last').append('<tr><td onmousedown="RowClick(event);" class="rawfiles_tbl_td_not_selected">' + filename_i + '</td></tr>');
   });
   trs = document.getElementById('rawfiles_tbl_allfiles').tBodies[0].getElementsByTagName('td');
}

var gen_expdesign = function (struct) {
   var ret = "";
   for (var i = 0; i < struct.length; i++) {
      ret = ret + struct[i].rawfile + "\t" + struct[i].biorep + "\t" + struct[i].techrep + "\t" + struct[i].fraction + "\n";
   }
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
      },
      success: function (data, textStatus, jqXHR) {
         renderRSSData(data, renderelem);
      },
      error: function (jqXHR, textStatus, errorThrown) {
      }
   });
}

var dlgFadeout = function () {
   $(".expparamsDlg").fadeOut(300, function () {
      $('#mask').remove();
   });
}


$(document).ready(function () {
   var forward_buttons = getItems("button.main", /s[0-9]+btnf/);
   var backward_buttons = getItems("button.main", /s[0-9]+btnb/);
   // Binds the click event to "toggleNextClass" for each "forward button" (button with class button.main and id /s[0-9]+btnf/) 
   forward_buttons.forEach(function (btn) {
      $(btn).on("click", function () {
         toggleNextClass(idsToStrings(".main_div .main_section"), "hidden", true, executeStage);
         if ($(backward_buttons[0]).hasClass("hidden")) {
            $(backward_buttons[0]).removeClass("hidden");
         }
      });
   });
   // Binds the click event to "toggleNextClass" for each "backward button" (button with class button.main and id /s[0-9]+btnb/) 
   backward_buttons.forEach(function (btn) {
      $(btn).on("click", function () {
         toggleNextClass(idsToStrings(".main_div .main_section").reverse(), "hidden", true, rollbackStage);
      });
   });
   // Bind event for file upload button
   $("#s2btnupld").on("click", function () {
      $("#__s2btnupld").click();
   });
   // Bind event when file(s) is/are chosen
   $("#__s2btnupld").change(function () {
      var fnames = "";
      $("#s2uluploaders > table").empty();
      if (this.files.length > 0) {
         //Start uploading ...
         uploadFiles(this.files, false, function(){
            if(++nUploaded == nToUpload && typeof rawfiles != 'undefined' && (peptideLabelsNamesFromFile.length > 0 || peptideLabelsFromFile.length > 0)){
              $("#s2btnf").prop('disabled', false);
            }
         });
      } else {
         $("#s2btnf").prop('disabled', true);
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
   //Add functionality of add/remove labels buttons
   $("#btnAddLabel").on("click", addFormLabel);
   $("#btnRemLabel").on("click", removeFormLabel);
   bind_explbldefinition_focus("#explbl" + nFormLabels + "definition");
   $("#dlglabelsBtnCancel").on("click", function () {
      $(".expparamsDlg").fadeOut(300, function () {
         $('#mask').remove();
      });
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
   $('.rawfiles_tbl').delegate('td', 'mouseenter', function () {
      var $this = $(this);
      if (this.offsetWidth < this.scrollWidth && !$this.attr('title')) {
         $this.attr('title', $this.text());
      }
   });
   //

   $('#btnAssignExpStructCoord').on("click", function () {
      var items = $('.rawfiles_tbl').find('.rawfiles_tbl_td_selected');
      var def_biorep = Number($('#expstructcoord_biorep').val());
      var def_techrep = Number($('#expstructcoord_techrep').val());
      var curr_biorep = def_biorep;
      var curr_techrep = def_techrep;
      var curr_fraction = 0;
      var rep_offset = 1;
      if (def_biorep > 0 && def_biorep > (bioreps + 1)) {	// return if user tries to assign biorep X without having defined biorep X-1
         return;
      }
      if (def_techrep > 0 && def_techrep > (techreps + 1)) {	// same as above but for techreps
         return;
      }
      if (def_biorep > 0) {
         if (def_techrep > 0) {
            $('#expstructcoord_techrep').val(def_techrep + 1);
         } else {
            $('#expstructcoord_biorep').val(def_biorep + 1);
         }
      }
      $("#btnResetExpStructCoord").prop('disabled', false);
      for (var i = 0; i < items.length; i++) {
         var curr_str = $(items[i]).html();
         if (curr_str.match(/\|/) == null) {
            if (def_biorep > 0) { // i.e. non-blank
               if (def_techrep == 0) { // blank
                  //techreps, from current count to items.length
                  if (def_biorep in rep_counts["biorep"]) {
                     curr_techrep = (rep_counts["biorep"][def_biorep].techrep.length - 1) + rep_offset++;
                  } else {
                     curr_techrep = rep_offset++;
                  }
               } else {
                  // fractions, from current count to items.length
                  if (def_biorep in rep_counts["biorep"] && def_techrep in rep_counts["biorep"][def_biorep].techrep) {
                     curr_fraction = (rep_counts["biorep"][def_biorep].techrep[def_techrep].fraction.length - 1) + rep_offset++;
                  } else {
                     curr_fraction = rep_offset++;
                  }
               }
            } else {
               if (def_techrep == 0) { // blank
                  // bioreps, from current count to items.length
                  curr_biorep = rep_counts["biorep"].length + rep_offset++;
               } else {
                  // error, a biorep must be specified
                  if (rawfiles_structure.length == 0) {
                     $("#btnResetExpStructCoord").prop('disabled', true);
                  }
                  return;
               }
            }
            rawfiles_structure.push({rawfile: curr_str, biorep: curr_biorep, techrep: curr_techrep, fraction: curr_fraction});
            curr_str = (curr_biorep == 0 ? '-' : curr_biorep) + ',' + (curr_techrep == 0 ? '-' : curr_techrep) + ',' + (curr_fraction == 0 ? '-' : curr_fraction) + ' | ' + curr_str;
            $(items[i]).html(curr_str);
         }
      }

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
      if (rawfiles_structure.length == rawfiles.length) {	//if all files have been assigned something
         $("#s22btnf").prop('disabled', false);
      }
   });
   $('#btnResetExpStructCoord').on("click", function () {
      reset_reps();
      $("#btnResetExpStructCoord").prop('disabled', true);
   });

   // CSS
   $(".tooltip").hover(function () {
      $(".tooltip span").css({"margin-left": -$(".tooltip span").width() / 2 + 9});
      $(".callout").css({"left": $(".tooltip span").width() / 2});
   });

   // Bind test datasets dialog buttons
   $("#dlgTestDatasetsBtnOK").on("click", function () {
      dlgFadeout();
      postTestDatasetInfo($("#s1TestDatasetsSelection option:selected").text());
   });
   $("#dlgTestDatasetsBtnCancel").on("click", function () {
      dlgFadeout();
   });
   $("#dlgTestDatasetsBtnDownload").on("click", function () {
      dlgFadeout();
      downloadTestDataset($("#s1TestDatasetsSelection option:selected").text());
   });


   // TEST DATA INIT
   postTestDatasetsInfo();
   //
   postClientServerClientInfo();
});





