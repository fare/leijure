// Copyright (c) 2014 Google, Inc.
// The use and distribution terms for this software are covered by the
// Apache License 2.0 http://www.apache.org/licenses/LICENSE-2.0.html
package com.google.leijure;

import static org.junit.Assert.assertEquals;

import com.google.leijure.LoadClojure;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.LinkedList;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/**
 * Trivial class to load and use Clojure in a Java application that doesn't include it by default.
 */
@RunWith(JUnit4.class)
public class LoadClojureTest {
    @Test
    public void testLoadClojure () throws Exception {
        LoadClojure lc = new LoadClojure();
        assertEquals(4L, lc.loadString("(+ 2 2)"));
    }
}
