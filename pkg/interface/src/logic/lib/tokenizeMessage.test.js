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
    const example = 'this is a url: http://tlon.io';
    const [{ text }, { url }] = tokenizeMessage(example);
    expect(text).toEqual('this is a url: ');
    expect(url).toEqual('http://tlon.io');
  });
  it('should ignore urls in codemode', () => {
    const example = 'some urls `http://ignore.me` http://urbit.org';
    const [{ text }, { url }] = tokenizeMessage(example);
    expect(text).toEqual('some urls `http://ignore.me` ');
    expect(url).toEqual('http://urbit.org');
  });

  it('should autoexpand group references', () => {
    const example = 'test ~bitbet-bolbel/urbit-community foo';
    const [{ text }, { reference }, { text: foo }] = tokenizeMessage(example);
    expect(text).toEqual('test ');
    expect(reference.group).toEqual('/ship/~bitbet-bolbel/urbit-community');
    expect(foo).toEqual(' foo');
  });

  it('should preserve trailing newlines and spaces', () => {
    const example = 'test \n \n foo \n \n \n';
    const [{ text }] = tokenizeMessage(example);
    expect(text).toEqual(example);
  });
});
