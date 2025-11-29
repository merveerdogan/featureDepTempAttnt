<?php
$post_data = json_decode(file_get_contents('php://input'), true);

// Extract folder name and filename from the posted data
$folderPath = $post_data['folder_name'];
$fileName = $post_data['filename'] . ".csv";
$data = $post_data['filedata'];

// Ensure the folder exists - create recursively if needed
if (!is_dir($folderPath)) {
    mkdir($folderPath, 0777, true); // Recursive creation with full permissions
}

// Construct the full path with filename, ensuring correct directory separators
$fullPath = $folderPath . DIRECTORY_SEPARATOR . $fileName;

// Write the file to disk
file_put_contents($fullPath, $data);
?>