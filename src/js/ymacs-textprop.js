DEFINE_CLASS("Ymacs_Text_Properties", DlEventProxy, function(D, P){

        D.DEFAULT_EVENTS = [ "onChange" ];

        D.CONSTRUCT = P.reset = function() {
                this.props = [];
        };

        // inserts n empty properties at position row
        P.insertLine = function(row, n) {
                if (n == null)
                        n = 1;
                if (this.props.length < row) {
                        this.props[row] = null;
                        --n;
                }
                if (n > 0) {
                        var a = new Array(n + 2);
                        a[0] = row;
                        a[1] = 0;
                        this.props.splice.apply(this.props, a);
                }
        };

        // removes n properties at position row
        P.deleteLine = function(row, n) {
                this.props.splice(row, n);
        };

        P.addLineProps = function(row, i, j, prop, val) {
                var p = this.props, o, changed = false;
                if (i < j) {
                        p = p[row] || (p[row] = []);
                        while (i < j) {
                                o = p[i] || (p[i] = {});
                                if (o[prop] != val)
                                        changed = true;
                                o[prop] = val;
                                ++i;
                        }
                        if (changed)
                                this.callHooks("onChange", row);
                }
                return changed;
        };

        P.removeLineProps = function(row, i, j, prop) {
                var p = this.props[row], o, changed = false;
                if (p && i < j) {
                        while (i < j) {
                                o = p[i];
                                if (o && prop in o) {
                                        changed = true;
                                        delete o[prop];
                                }
                                ++i;
                        }
                        if (changed)
                                this.callHooks("onChange", row);
                }
                return changed;
        };

        // this uses the "css" text property to intercalate <span
        // class="$css"> ... </span> tags in the given text.  "css"
        // properties are added as the tokenizer parses the code and
        // sends onFoundToken events.
        P.getLineHTML = function(row, text) {
                var p = this.props[row];
                if (text == "")
                        return "<br/>";
                if (!p || p.length == 0)
                        return text.htmlEscape();
                var i = 0, n = text.length, last = null, o, ret = "";
                while (i < n) {
                        o = p[i];
                        o = o && o.css;
                        if (o && o != last) {
                                if (last)
                                        ret += "</span>";
                                ret += "<span class='" + o + "'>";
                        }
                        else if (!o && last) {
                                ret += "</span>";
                        }
                        last = o;
                        ret += text.charAt(i);
                        ++i;
                }
                if (last)
                        ret += "</span>";
                return ret;
        };

});

// function test_tx(){
//         var text = "check this out";
//         var tp = new Ymacs_Text_Properties({});
//         tp.addLineProps(0, 6, 10, "css", "keyword");
//         tp.addLineProps(0, 11, 14, "css", "crap");
//         tp.addLineProps(0, 0, 5, "css", "test");
//         tp.addLineProps(0, 2, 3, "css", null);
//         console.time("test");
//         text = tp.getLineHTML(0, text);
//         console.timeEnd("test");
//         console.log(text);
// };
