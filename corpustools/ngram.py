import traceback
from exceptions import KeyboardInterrupt
import os
import glob
import sys

nb_ngrams = 400


class _NGram:
    def __init__(self, arg={}):
        t = type(arg)
        if t == type(""):
            self.addText(arg)
            self.normalise()
        elif t == type({}):
            self.ngrams = arg
            self.normalise()
        else:
            raise TypeError("arg should be either a text or a (possibly empty) ngram-dict")

    def addText(self, text):
        ngrams = dict()
        words = text.split()
        for word in words:
            word = '_'+word+'_'
            size = len(word)
            for i in range(size):
                for s in (1, 2, 3, 4):
                    sub = word[i:i+s]
                    ngrams[sub] = ngrams.get(sub, 0) + 1
                    if i+s >= size:
                        break
        self.ngrams = ngrams
        return self

    def sorted(self):
        sorted = [(v, k) for k, v in self.ngrams.items()]
        sorted.sort()
        sorted.reverse()
        sorted = sorted[:nb_ngrams]
        return sorted

    def normalise(self):
        count = 0
        ngrams = dict()
        for v, k in self.sorted():
            ngrams[k] = count
            count += 1
        self.ngramskeyset = set(ngrams.keys())
        self.ngrams = ngrams
        self.ngrams_ranks = {
            gram:rank
            for rank, (gram, freq)
            in enumerate(sorted(ngrams.iteritems(), key=lambda x:x[1]))
        }
        return self

    def addValues(self, key, value):
        # TODO: unused, remove?
        self.ngrams[key] = value
        return self

    def compare(self, ngram):
        settolookout = self.ngramskeyset.intersection(ngram.ngramskeyset)
        missingcount = len(self.ngramskeyset) - len(settolookout)
        missingcount = len([x for x in ngram.ngramskeyset if x not in self.ngramskeyset])
        d = missingcount * nb_ngrams
        for k in settolookout:
            old_d = d
            d += abs(ngram.ngrams[k] - self.ngrams[k])
            #print "d=%d += %d gives %d (k=%s)" %(old_d, abs(ngram.ngrams[k] - self.ngrams[k]), d, k)
        #print ""
        return d

    def rank_compare(self, ngram):
        not_found_value = nb_ngrams
        return sum(
            abs(rank - self.ngrams_ranks.get(gram, not_found_value))
            for gram,rank
            in ngram.ngrams_ranks.iteritems()
        )


class NGram:
    def __init__(self, folder, ext='.lm', langs=[]):
        self.ngrams = dict()

        size = len(ext)
        count = 0

        fnames = []
        if len(langs) == 0:
            folder = os.path.join(folder, '*'+ext)
            fnames = glob.glob(os.path.normcase(folder))
        else:
            for lang in langs:
                fnames.append(os.path.join(folder, lang+ext))

        for fname in fnames:
            count += 1
            lang = os.path.split(fname)[-1][:-size]
            ngrams = dict()
            try:
                file = open(fname, 'r')

                for line in file.readlines():
                    parts = line.strip().split('\t ')
                    if len(parts) != 2:
                        raise ValueError("invalid language file %s line : %s"
                                         % (fname, parts))
                    try:
                        ngrams[parts[0]] = int(parts[1])
                    except KeyboardInterrupt:
                        raise
                    except:
                        traceback.print_exc()
                        raise ValueError("invalid language file %s line : %s"
                                         % (fname, parts))

                if len(ngrams.keys()):
                    self.ngrams[lang] = _NGram(ngrams)

                file.close()
            except IOError:
                sys.stderr.write("Unknown language: " +
                                 os.path.basename(fname)[:-len(ext)] +
                                 ' (Language recognition)\n')

        if not count:
            raise ValueError("no language files found")

    def classify_full(self, text):
        if len(self.ngrams) == 0:
            return [('guess', 0)]
        else:
            ingram = _NGram(text)
            return sorted(
                [(l, compare(self.ngrams[l], ingram))
                 for l in self.ngrams],
                key=lambda x:x[1]
            )

    def classify(self, text):
        return self.classify_full(text)[0][0]

    def rank_classify_full(self, text):
        if len(self.ngrams) == 0:
            return [('guess', 0)]
        else:
            ingram = _NGram(text)
            return sorted(
                [(l, self.ngrams[l].rank_compare(ingram))
                 for l in self.ngrams],
                key=lambda x:x[1]
            )

    def rank_classify(self, text):
        return self.rank_classify_full(text)[0][0]


class Generate:
    def __init__(self, folder, ext='.txt'):
        self.ngrams = dict()
        folder = os.path.join(folder, '*'+ext)
        size = len(ext)
        count = 0

        for fname in glob.glob(os.path.normcase(folder)):
            count += 1
            lang = os.path.split(fname)[-1][:-size]
            n = _NGram()

            file = open(fname, 'r')
            for line in file.readlines():
                n.addText(line)
            file.close()

            n.normalise()
            self.ngrams[lang] = n

    def save(self, folder, ext='.lm'):
        for lang in self.ngrams.keys():
            fname = os.path.join(folder, lang+ext)
            file = open(fname, 'w')
            for v, k in self.ngrams[lang].sorted():
                file.write("%s\t %d\n" % (k, v))
            file.close()

def compare(ngram1, ngram):
    settolookout = ngram1.ngramskeyset.intersection(ngram.ngramskeyset)
    missingcount = len(ngram1.ngramskeyset) - len(settolookout)
    #missingcount = len([x for x in ngram.ngramskeyset if x not in ngram1.ngramskeyset])
    d = missingcount * nb_ngrams
    for k in settolookout:
        old_d = d
        d += abs(ngram.ngrams[k] - ngram1.ngrams[k])
        #print "d=%d += %d gives %d (k=%s)" %(old_d, abs(ngram.ngrams[k] - ngram1.ngrams[k]), d, k)
    #print ""
    return d

        #ng = _NGram()
#lg = NGram('/home/vsk/Desktop/Langdet/san/ngram_language/LM/')

#def detectLanguage(text):
    #global lg
    #text = text.encode("ascii", "ignore")
    #return lg.classify(text)
ng = NGram(os.path.join(os.getenv('GTHOME'), 'tools/lang-guesser/LM/'), langs=["sme", "smj"])
ng.ngrams={'smj':new_smj_model, 'sme':new_sme_model}
print ""
for lang in ['smj', 'sme']:
    lines=open('/tmp/'+lang+'.test','r').readlines()
    res=[ng.rank_classify(line) for line in lines]
    print "rank", lang, "correct", len([l for l in res if l==lang]), "wrong",len([l for l in res if l!=lang])
    res=[ng.classify(line) for line in lines]
    print "norm", lang, "correct", len([l for l in res if l==lang]), "wrong", len([l for l in res if l!=lang])
