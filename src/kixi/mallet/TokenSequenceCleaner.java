package kixi.mallet;

import java.io.ObjectOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.regex.Pattern;

import cc.mallet.types.FeatureSequenceWithBigrams;
import cc.mallet.types.Instance;
import cc.mallet.types.Token;
import cc.mallet.types.TokenSequence;
import cc.mallet.util.CharSequenceLexer;
import cc.mallet.pipe.Pipe;

/* Copyright (C) 2005 Univ. of Massachusetts Amherst, Computer Science Dept.
This file is part of "MALLET" (MAchine Learning for LanguagE Toolkit).
http://www.cs.umass.edu/~mccallum/mallet
This software is provided under the terms of the Common Public License,
version 1.0, as published by http://www.opensource.org.  For further
information, see the file `LICENSE' included with this distribution. */

/* Remove tokens that do not match passed in Regex */
public class TokenSequenceCleaner extends cc.mallet.pipe.Pipe {
  boolean markDeletions = false;
  Pattern acceptPattern;

  public TokenSequenceCleaner (boolean markDeletions, Pattern regex)
  {
    this.markDeletions = markDeletions;
    this.acceptPattern = regex;
  }

  public TokenSequenceCleaner (Pattern regex)
  {
    this (false, regex);
  }

  public TokenSequenceCleaner ()
  {
    this (false, CharSequenceLexer.LEX_ALPHA);
  }

  public Instance pipe (Instance carrier)
  {
    TokenSequence ts = (TokenSequence) carrier.getData();
    TokenSequence ret = new TokenSequence ();
    Token prevToken = null;
    for (int i = 0; i < ts.size(); i++) {
      Token t = ts.get(i);
      String s = t.getText();
      if (this.acceptPattern.matcher(s).matches()) {
        ret.add (t);
        prevToken = t;
      } else if (markDeletions && prevToken != null)
        prevToken.setProperty (FeatureSequenceWithBigrams.deletionMark, t.getText());
    }
    carrier.setData(ret);
    return carrier;
  }

  // Serialization

  private static final long serialVersionUID = 1;
  private static final int CURRENT_SERIAL_VERSION = 0;

  private void writeObject (ObjectOutputStream out) throws IOException {
    out.writeInt (CURRENT_SERIAL_VERSION);
    out.writeBoolean(markDeletions);
  }

  private void readObject (ObjectInputStream in) throws IOException, ClassNotFoundException {
    int version = in.readInt ();
    markDeletions = in.readBoolean();
  }

}
