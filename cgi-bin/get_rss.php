 <?php
 // function from http://php.net/manual/en/class.simplexmlelement.php
 function xml2obj($xml,$force = false){

    $obj = new StdClass();    

    $obj->name = $xml->getName();
    
    $text = trim((string)$xml);
    $attributes = array();
    $children = array();
    
    foreach($xml->attributes() as $k => $v){
        $attributes[$k]  = (string)$v;
    }
    
    foreach($xml->children() as $k => $v){
        $children[] = xml2obj($v,$force);
    }
    
    
    if($force or $text !== '')
        $obj->text = $text;
        
    if($force or count($attributes) > 0)
        $obj->attributes = $attributes;
        
    if($force or count($children) > 0)
        $obj->children = $children;
        
        
    return $obj;
}
/*
 $xml=($_POST["rssurl"]);
	$feed = new DOMDocument();
	$feed->load($xml);
	 $json = array();
	 $json['title'] = $feed->getElementsByTagName('channel')->item(0)->getElementsByTagName('title')->item(0)->firstChild->nodeValue;
	 $json['description'] = $feed->getElementsByTagName('channel')->item(0)->getElementsByTagName('description')->item(0)->firstChild->nodeValue;
	 $json['link'] = $feed->getElementsByTagName('channel')->item(0)->getElementsByTagName('link')->item(0)->firstChild->nodeValue;
	 $items = $feed->getElementsByTagName('channel')->item(0)->getElementsByTagName('item');
	 $json['item'] = array();
	 $i = 0;
	 foreach($items as $key => $item) {
	 $title = $item->getElementsByTagName('title')->item(0)->firstChild->nodeValue;
	 $description = $item->getElementsByTagName('description')->item(0)->firstChild->nodeValue;
	 $pubDate = $item->getElementsByTagName('pubDate')->item(0)->firstChild->nodeValue;
	 $guid = $item->getElementsByTagName('guid')->item(0)->firstChild->nodeValue;

	 $json['item'][$key]['title'] = $title;
	 $json['item'][$key]['description'] = $description;
	 $json['item'][$key]['pubdate'] = $pubDate;
	 $json['item'][$key]['guid'] = $guid; 
	 }
*/
	$server_response = [];
	$xml = xml2obj(simplexml_load_file($_POST["rssurl"]), true);
	foreach($xml->children as $item)
	{
		// title (children[0]) and link (children[1]) nodes values
		$server_response[$item->children[0]->text] = $item->children[1]->text;
	}
	header('Content-type: application/json');
	echo json_encode($server_response);
 ?>