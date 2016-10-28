package mikera.orculje.demo;

import mikera.cljutils.Clojure;

public class DemoApp {

	public static void main(String[] args) {
		// just launch the Clojure demo
		Clojure.require("mikera.orculje.demo.main");
		Clojure.eval("(mikera.orculje.demo.main/-main)");
	}
}
