```
$ git config --global user.name "John Doe"
$ git config --global user.email johndoe@example.com
$
```

```
$ mkdir ~/git-intro && cd ~/git-intro
$ git init
Initialized empty Git repository in /Users/jonasmatser/Development/git-intro/.git/
$
```

```
$ touch README.md
$ git status
On branch masterInitial commit

Untracked files:
  (use "git add <file>..." to include in what will be committed)

	README.md

nothing added to commit but untracked files present (use "git add" to track)

$
```

```
$ git add README.md
$ git status
On branch master

Initial commit

Changes to be committed:
  (use "git rm --cached <file>..." to unstage)

	new file:   README.md

$
```

```
$ git commit –m ‘First commit!’
[master (root-commit) 104c973] First commit!
 1 file changed, 0 insertions(+), 0 deletions(-)
 create mode 100644 README.md

$ git status
On branch master
nothing to commit, working tree clean
$
```

```
$ git log
commit 39ce38c5fcdbd6874e043af3cb89cdb3ed156544
Author: Jonas Matser <jonas@jonasmatser.nl>
Date:   Thu Jan 25 22:25:53 2018 +0100

    Initial commit
$
```

```
$ echo –e ‘# Hello world!\n’ > README.md
$ git diff
diff --git a/README.md b/README.md
index e69de29..f2aa061 100644
--- a/README.md
+++ b/README.md
@@ -0,0 +1 @@
+# Hello World!
+
$
```

```
$ git stash
Saved working directory and index state WIP on master: 104c973 First commit!
HEAD is now at 104c973 First commit!
$
```

```
$ git stash pop
On branch master
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

	modified:   README.md

no changes added to commit (use "git add" and/or "git commit -a")
Dropped refs/stash@{0} (0a84ab1bfc512bb6dca6be0a95020a64d022453b)
$
```

```
$ git clone https://github.com/MariekeDirk/Reproducibility-Reusability-course.git
Cloning into 'Reproducibility-Reusability-course'...
remote: Counting objects: 112, done.
remote: Total 112 (delta 0), reused 0 (delta 0), pack-reused 112
Receiving objects: 100% (112/112), 821.36 KiB | 0 bytes/s, done.
Resolving deltas: 100% (55/55), done.
$
```

```
$ git push
Counting objects: 3, done.
Delta compression using up to 4 threads.
Compressing objects: 100% (3/3), done.
Writing objects: 100% (3/3), 324 bytes | 0 bytes/s, done.
Total 3 (delta 2), reused 0 (delta 0)
remote: Resolving deltas: 100% (2/2), completed with 2 local objects.
To github.com:mtsr/Reproducibility-Reusability-course.git
   8656977..3f15e15  master -> master
$
```

```
$ ssh-keygen
Generating public/private rsa key pair.
Enter file in which to save the key (/root/.ssh/id_rsa):
Created directory '/root/.ssh'.
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in /Users/jonasmatser/.ssh/id_rsa.
Your public key has been saved in /Users/jonasmatser/.ssh/id_rsa.pub.
<snip>

$ cat ~/.ssh/id_rsa.pub
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDtbOHTZtV5mNHQEgqJ20IgjGh2vilbQKnLM740hbSZeixJeESLVk2MH38BkCV2qUKM0u/R69q+H4AFRtCoKMCBmb05IIPM+KZmUW4fHMFQpxq0J0fVhkgNbDbysqHsBx64vNoLG41hbCfBnIO4eaKOxJVgso9Mk3ct2IUTSqn3jb7gr10ugruiUmTtTq3ubCmBKnrUNXVKWochcnKNlFkvNVhZ5hB7IT4SM2TaylTwUm9+jqX8NhQukQWC9raCHs0+LeSH4GY2nS9LoxJvkhBASteYRE4sWVMG4++8uIhZ8ciOARYdZAPJ7OQQTDH9g3Z2V/r3OHzVBLVLiOSQZ3UV jonasmatser@Jonass-MacBook-Pro.local
$
```