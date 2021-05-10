import remark from 'remark';
import RemarkDisableTokenizers from 'remark-disable-tokenizers';

const DISABLED_BLOCK_TOKENS = [
  'indentedCode',
  'atxHeading',
  'thematicBreak',
  'list',
  'setextHeading',
  'html',
  'definition',
  'table',
];

const DISABLED_INLINE_TOKENS = ['autoLink', 'url', 'email', 'reference'];

const tallParser = remark().freeze();

export const parseTall = (text: string) => tallParser.parse(text);

const wideParser = remark()
  .use([
    [
      RemarkDisableTokenizers,
      {
        block: DISABLED_BLOCK_TOKENS,
        inline: DISABLED_INLINE_TOKENS,
      },
    ],
  ])
  .freeze();

export const parseWide = (text: string) => wideParser.parse(text);
