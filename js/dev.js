// Call the following from postParameters in script.js for generating an SQL script for importing the exp. info into testdatadb

function dumpExpParamSQL(tmp){
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
   link.href = window.URL.createObjectURL(new Blob([sqlscript.join("\n")], {type: 'text/plain'}));
   link.click();
}
