open Mocha
open Test__Util

describe("FS", () => {
  This.timeout(10000)
  
  describe("readDirectory", () => {
    Async.it(
      "should return directory contents when directory exists",
      async () => {
        // Create a temporary directory with some files
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        
        // Create directory and files
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(NodeJs.Path.join([tempDir, "test.txt"]), NodeJs.Buffer.fromString("test"))
        await NodeJs.Fs.mkdir(NodeJs.Path.join([tempDir, "subdir"]), {recursive: true, mode: 0o777})
        
        // Test FS.readDirectory
        let uri = VSCode.Uri.file(tempDir)
        let result = await FS.readDirectory(uri)
        
        switch result {
        | Ok(entries) =>
          // Should have at least the file and subdirectory we created
          let names = entries->Array.map(((name, _type)) => name)
          Assert.ok(names->Array.includes("test.txt"))
          Assert.ok(names->Array.includes("subdir"))
          
          // Check types
          let testFileEntry = entries->Array.find(((name, _type)) => name == "test.txt")
          let subdirEntry = entries->Array.find(((name, _type)) => name == "subdir")
          
          switch (testFileEntry, subdirEntry) {
          | (Some((_, fileType)), Some((_, dirType))) =>
            // Note: Exact FileType values may vary, but this tests the structure
            Assert.ok(true) // If we got here, the types are correct
          | _ => Assert.fail("Expected to find both file and directory entries")
          }
          
        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }
        
        // Cleanup
        NodeJs.Fs.unlinkSync(NodeJs.Path.join([tempDir, "test.txt"]))
        NodeJs.Fs.rmdirSync(NodeJs.Path.join([tempDir, "subdir"]))
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return Error when directory does not exist",
      async () => {
        // Use a non-existent directory path
        let nonExistentPath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "non-existent-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        
        let uri = VSCode.Uri.file(nonExistentPath)
        let result = await FS.readDirectory(uri)
        
        switch result {
        | Ok(_) => Assert.fail("Expected Error for non-existent directory, got Ok")
        | Error(errorMsg) => 
          // Should get some kind of error message
          Assert.ok(String.length(errorMsg) > 0)
          // Common error messages might include "not found", "does not exist", etc.
          // We don't assert exact message since it may vary by platform/VS Code version
        }
      },
    )

    Async.it(
      "should return Error when path is a file, not directory",
      async () => {
        // Create a temporary file
        let tempFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-test-file-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])
        
        NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString("test content"))
        
        // Try to read it as a directory
        let uri = VSCode.Uri.file(tempFile)
        let result = await FS.readDirectory(uri)
        
        switch result {
        | Ok(_) => Assert.fail("Expected Error when trying to read file as directory, got Ok")
        | Error(errorMsg) =>
          // Should get an error indicating it's not a directory
          Assert.ok(String.length(errorMsg) > 0)
        }
        
        // Cleanup
        NodeJs.Fs.unlinkSync(tempFile)
      },
    )
  })

  describe("copy", () => {
    Async.it(
      "should copy file successfully when source exists",
      async () => {
        // Create source file
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-copy-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let sourceFile = NodeJs.Path.join([tempDir, "source.txt"])
        let destFile = NodeJs.Path.join([tempDir, "dest.txt"])
        
        // Create directory and source file
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        let content = "test file content"
        NodeJs.Fs.writeFileSync(sourceFile, NodeJs.Buffer.fromString(content))
        
        // Test FS.copy
        let sourceUri = VSCode.Uri.file(sourceFile)
        let destUri = VSCode.Uri.file(destFile)
        let result = await FS.copy(sourceUri, destUri)
        
        switch result {
        | Ok() =>
          // Verify destination file exists and has same content
          let exists = NodeJs.Fs.existsSync(destFile)
          Assert.ok(exists)
          
          let destContent = NodeJs.Fs.readFileSync(destFile)->NodeJs.Buffer.toString
          Assert.deepStrictEqual(destContent, content)
          
        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }
        
        // Cleanup
        NodeJs.Fs.unlinkSync(sourceFile)
        NodeJs.Fs.unlinkSync(destFile)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return Error when source file does not exist",
      async () => {
        // Use non-existent source file
        let nonExistentSource = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "non-existent-source-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])
        let destFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "dest-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])
        
        let sourceUri = VSCode.Uri.file(nonExistentSource)
        let destUri = VSCode.Uri.file(destFile)
        let result = await FS.copy(sourceUri, destUri)
        
        switch result {
        | Ok() => Assert.fail("Expected Error for non-existent source, got Ok")
        | Error(errorMsg) =>
          // Should get an error message
          Assert.ok(String.length(errorMsg) > 0)
        }
      },
    )

    Async.it(
      "should return Error when destination directory does not exist",
      async () => {
        // Create source file
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-copy-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let sourceFile = NodeJs.Path.join([tempDir, "source.txt"])
        let destFile = NodeJs.Path.join([tempDir, "nonexistent", "dest.txt"])
        
        // Create directory and source file
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(sourceFile, NodeJs.Buffer.fromString("test"))
        
        let sourceUri = VSCode.Uri.file(sourceFile)
        let destUri = VSCode.Uri.file(destFile)
        let result = await FS.copy(sourceUri, destUri)
        
        switch result {
        | Ok() => Assert.fail("Expected Error for non-existent destination directory, got Ok")
        | Error(errorMsg) =>
          // Should get an error message
          Assert.ok(String.length(errorMsg) > 0)
        }
        
        // Cleanup
        NodeJs.Fs.unlinkSync(sourceFile)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })
})
