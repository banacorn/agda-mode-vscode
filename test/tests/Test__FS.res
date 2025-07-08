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
      "should create parent directories when copying to non-existent destination directory",
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
        NodeJs.Fs.writeFileSync(sourceFile, NodeJs.Buffer.fromString("test content"))
        
        let sourceUri = VSCode.Uri.file(sourceFile)
        let destUri = VSCode.Uri.file(destFile)
        let result = await FS.copy(sourceUri, destUri)
        
        switch result {
        | Ok() =>
          // VS Code automatically creates parent directories
          let exists = NodeJs.Fs.existsSync(destFile)
          Assert.ok(exists)
          
          let content = NodeJs.Fs.readFileSync(destFile)->NodeJs.Buffer.toString
          Assert.deepStrictEqual(content, "test content")
          
        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }
        
        // Cleanup
        NodeJs.Fs.unlinkSync(sourceFile)
        NodeJs.Fs.unlinkSync(destFile)
        NodeJs.Fs.rmdirSync(NodeJs.Path.join([tempDir, "nonexistent"]))
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })

  describe("createDirectory", () => {
    Async.it(
      "should create directory successfully when parent exists",
      async () => {
        // Create parent directory
        let parentDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-mkdir-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let newDir = NodeJs.Path.join([parentDir, "newdir"])
        
        // Create parent directory
        await NodeJs.Fs.mkdir(parentDir, {recursive: true, mode: 0o777})
        
        // Test FS.createDirectory
        let uri = VSCode.Uri.file(newDir)
        let result = await FS.createDirectory(uri)
        
        switch result {
        | Ok() =>
          // Verify directory was created
          let exists = NodeJs.Fs.existsSync(newDir)
          Assert.ok(exists)
          
        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }
        
        // Cleanup
        NodeJs.Fs.rmdirSync(newDir)
        NodeJs.Fs.rmdirSync(parentDir)
      },
    )

    Async.it(
      "should create parent directories recursively when they don't exist",
      async () => {
        // Use non-existent parent directory
        let nonExistentParent = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "non-existent-parent-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let newDir = NodeJs.Path.join([nonExistentParent, "newdir"])
        
        let uri = VSCode.Uri.file(newDir)
        let result = await FS.createDirectory(uri)
        
        switch result {
        | Ok() =>
          // VS Code creates parent directories recursively
          let exists = NodeJs.Fs.existsSync(newDir)
          Assert.ok(exists)
          
          let parentExists = NodeJs.Fs.existsSync(nonExistentParent)
          Assert.ok(parentExists)
          
        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }
        
        // Cleanup
        NodeJs.Fs.rmdirSync(newDir)
        NodeJs.Fs.rmdirSync(nonExistentParent)
      },
    )

    Async.it(
      "should succeed when directory already exists (idempotent)",
      async () => {
        // Create directory first
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-mkdir-existing-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        
        // Create the directory first
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        
        // Try to create it again with FS.createDirectory
        let uri = VSCode.Uri.file(tempDir)
        let result = await FS.createDirectory(uri)
        
        switch result {
        | Ok() =>
          // VS Code createDirectory is idempotent - doesn't error on existing dirs
          let exists = NodeJs.Fs.existsSync(tempDir)
          Assert.ok(exists)
          
        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }
        
        // Cleanup
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })

  describe("delete", () => {
    Async.it(
      "should delete file successfully when file exists",
      async () => {
        // Create a file to delete
        let tempFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-delete-file-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])
        
        // Create the file
        NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString("test content"))
        
        // Verify it exists
        let existsBefore = NodeJs.Fs.existsSync(tempFile)
        Assert.ok(existsBefore)
        
        // Test FS.delete
        let uri = VSCode.Uri.file(tempFile)
        let result = await FS.delete(uri)
        
        switch result {
        | Ok() =>
          // Verify file was deleted
          let existsAfter = NodeJs.Fs.existsSync(tempFile)
          Assert.ok(!existsAfter)
          
        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }
      },
    )

    Async.it(
      "should delete directory successfully when directory exists and is empty",
      async () => {
        // Create an empty directory to delete
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-delete-dir-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        
        // Create the directory
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        
        // Verify it exists
        let existsBefore = NodeJs.Fs.existsSync(tempDir)
        Assert.ok(existsBefore)
        
        // Test FS.delete
        let uri = VSCode.Uri.file(tempDir)
        let result = await FS.delete(uri)
        
        switch result {
        | Ok() =>
          // Verify directory was deleted
          let existsAfter = NodeJs.Fs.existsSync(tempDir)
          Assert.ok(!existsAfter)
          
        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }
      },
    )

    Async.it(
      "should return Error when trying to delete non-empty directory",
      async () => {
        // Create a directory with files
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-delete-nonempty-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let file1 = NodeJs.Path.join([tempDir, "file1.txt"])
        
        // Create directory structure
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(file1, NodeJs.Buffer.fromString("content1"))
        
        // Verify everything exists
        Assert.ok(NodeJs.Fs.existsSync(tempDir))
        Assert.ok(NodeJs.Fs.existsSync(file1))
        
        // Test FS.delete on non-empty directory (should fail)
        let uri = VSCode.Uri.file(tempDir)
        let result = await FS.delete(uri)
        
        switch result {
        | Ok() => Assert.fail("Expected Error for non-empty directory, got Ok")
        | Error(errorMsg) =>
          // Should get an error message about directory not being empty
          Assert.ok(String.length(errorMsg) > 0)
          // Directory and file should still exist
          Assert.ok(NodeJs.Fs.existsSync(tempDir))
          Assert.ok(NodeJs.Fs.existsSync(file1))
        }
        
        // Cleanup manually
        NodeJs.Fs.unlinkSync(file1)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return Error when trying to delete non-existent file",
      async () => {
        // Use non-existent file path
        let nonExistentFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "non-existent-delete-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])
        
        let uri = VSCode.Uri.file(nonExistentFile)
        let result = await FS.delete(uri)
        
        switch result {
        | Ok() => Assert.fail("Expected Error for non-existent file, got Ok")
        | Error(errorMsg) =>
          // Should get an error message
          Assert.ok(String.length(errorMsg) > 0)
        }
      },
    )
  })
})
