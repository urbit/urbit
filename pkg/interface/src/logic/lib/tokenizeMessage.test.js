/* eslint-disable max-lines-per-function */
import tokenizeMessage from './tokenizeMessage';

describe('tokenizeMessage', () => {
  it('should find mention with punctuation', () => {
    const [before, mention, after] = tokenizeMessage('hi ~hastuc-dibtux!');

    expect(before.text).toEqual('hi ');
    expect(mention.mention).toEqual('~hastuc-dibtux');
    expect(after.text).toEqual('!');
  });
  it('should not parse mention inside inline code', () => {
    const example = 'hi `~hastuc-dibtux`';
    const [first] = tokenizeMessage(example);
    expect(first.text).toEqual(example);
    const secondExample = 'foo`~hastuc-dibtux`bar`baz`~hastuc-dibtux';

    const [{ text }, { mention }] = tokenizeMessage(secondExample);
    expect(text).toEqual('foo`~hastuc-dibtux`bar`baz`');
    expect(mention).toEqual('~hastuc-dibtux');
  });
  it('should parse urls', () => {
    const example = 'this is a url: http://tlon.io/';
    const [{ text }, { url }] = tokenizeMessage(example);
    expect(text).toEqual('this is a url: ');
    expect(url).toEqual('http://tlon.io/');
  });
  it('should ignore urls in codemode', () => {
    const example = 'some urls `http://ignore.me` http://urbit.org';
    const [{ text }, { url }] = tokenizeMessage(example);
    expect(text).toEqual('some urls `http://ignore.me` ');
    expect(url).toEqual('http://urbit.org');
  });

  it('should autoexpand group references', () => {
    const example = 'test ~bitbet-bolbel/urbit-community foo';
    const result = tokenizeMessage(example);
    const [{ text }, { reference }, { text: foo }] = result;
    expect(text).toEqual('test ');
    expect(reference.group).toEqual('/ship/~bitbet-bolbel/urbit-community');
    expect(foo).toEqual(' foo');
  });

  it('should preserve trailing newlines and spaces', () => {
    const example = 'test \n \n foo \n \n \n';
    const [{ text }] = tokenizeMessage(example);
    expect(text).toEqual(example);
  });

  it('should handle multiline messages with references', () => {
    const example = 'web+urbitgraph://group/~fabled-faster/interface-testing-facility/graph/~hastuc-dibtux/test-book-7531/170141184505064871297992714192687202304\n\nlol here [is a link](https://urbit.org)';
    const [{ reference }, { text: text2 }] = tokenizeMessage(example);
    expect(reference.graph.graph).toEqual('/ship/~hastuc-dibtux/test-book-7531');
    expect(reference.graph.index).toEqual('/170141184505064871297992714192687202304');
    expect(text2).toEqual('\n\nlol here [is a link](https://urbit.org)');
  });

  it('should handle links on newlines after references', () => {
    const example = 'web+urbitgraph://group/~fabled-faster/interface-testing-facility/graph/~hastuc-dibtux/test-book-7531/170141184505064871297992714192687202304\n\nhttps://urbit.org a link is here!';
    const [{ reference }, { text: text2 }, { url }, { text: text3 }] = tokenizeMessage(example);
    expect(reference.graph.graph).toEqual('/ship/~hastuc-dibtux/test-book-7531');
    expect(reference.graph.index).toEqual('/170141184505064871297992714192687202304');
    expect(text2).toEqual('\n\n');
    expect(url).toEqual('https://urbit.org');
    expect(text3).toEqual(' a link is here!');
  });
  it('should tokenize mention at start of a line', () => {
    const example = '~haddef-sigwen test';
    const result = tokenizeMessage(example);
    const [{ mention }, { text }] = result;
    expect(mention).toEqual('~haddef-sigwen');
    expect(text).toEqual(' test');
  });
  it('should tokenize both mentions and links', () => {
    const example = '~haddef-sigwen have you looked at https://urbit.org lately?';
    const result = tokenizeMessage(example);
    const [{ mention }, { text }, { url }, { text: text2 }] = result;
    expect(mention).toEqual('~haddef-sigwen');
    expect(text).toEqual(' have you looked at ');
    expect(url).toEqual('https://urbit.org');
    expect(text2).toEqual(' lately?');
  });
});
