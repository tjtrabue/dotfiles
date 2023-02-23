# Professional Documents

This is my repository for professional documents, such as my collection of
resumes.

## Editing the Encrypted Resume (`resume.tex.gpg`)

The resume contains semi-sensitive personal information. In order to safeguard
these details, only commit the encrypted version of the resume to the Git
repository. However, although Emacs is great at working with GPG-encrypted files
(using EasyPG), getting a live-preview of an encrypted TeX file is virtually
impossible. Thus, for ease of modification, I recommend decrypting the resume
file, working with the decrypted version in Emacs, and then, once you are ready
to commit a new version, use GPG to encrypt the resume using this command:

```shell
# Use standard AES256 symmetric encryption.
gpg --batch --output resume.tex.gpg --passphrase <passphrase> \
  --symmetric resume.tex
```
