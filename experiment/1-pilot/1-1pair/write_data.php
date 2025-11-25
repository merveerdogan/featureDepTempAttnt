<?php
$post_data = json_decode(file_get_contents('php://input'), true);

// Extract folder name and filename from the posted data
$folderPath = $post_data['folder_name'];
$fileName = $post_data['filename'] . ".csv";
$data = $post_data['filedata'];

// Ensure the folder name ends with a directory separator for proper path construction
if (!is_dir($folderPath)) {
    mkdir($folderPath); // Use default permissions
}

// Construct the full path with filename, ensuring correct directory separators
$fullPath = $folderPath . DIRECTORY_SEPARATOR . $fileName;

// Write the file to disk
file_put_contents($fullPath, $data);
?>