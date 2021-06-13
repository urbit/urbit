import remark from 'remark';
import RemarkDisableTokenizers from 'remark-disable-tokenizers';
import newlines from './remark-break';
import math from 'remark-math';
import inlineMath from 'remark-math/inline';

export interface ParserSettings {
  inList: boolean;
  inBlock: boolean;
  inLink: boolean;
}

const DISABLED_BLOCK_TOKENS = [
  'indentedCode',
  'atxHeading',
  'thematicBreak',
  'list',
  'setextHeading',
  'html',
  'definition',
  'table'
];

const DISABLED_INLINE_TOKENS = ['autoLink', 'url', 'email', 'reference', 'html'];

const tallParser = remark().use(math).freeze();

export const parseTall = (text: string) => tallParser.parse(text);

const wideParser = remark()
  .use([
    [
      RemarkDisableTokenizers,
      {
        block: DISABLED_BLOCK_TOKENS,
        inline: DISABLED_INLINE_TOKENS
      }
    ],
    newlines
  ])
  .use(inlineMath);

export const parseWide = (text: string) => wideParser.parse(text);
