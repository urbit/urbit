import { Notebooks, Notebook, Note, BookId, NoteId } from './publish-update';
import { Patp } from './noun';

export type PublishResponse =
  NotebooksResponse
| NotebookResponse
| NoteResponse
| NotesPageResponse
| CommentsPageResponse;

interface NotebooksResponse {
  type: 'notebooks';
  data: Notebooks;
}

interface NotebookResponse {
  type: 'notebook';
  data: Notebook;
  host: Patp;
  notebook: BookId;
}

interface NoteResponse {
  type: 'note';
  data: Note;
  host: Patp;
  notebook: BookId;
  note: NoteId;
}

interface NotesPageResponse {
  type: 'notes-page';
  data: Note[];
  host: Patp;
  notebook: BookId;
  startIndex: number;
  length: number;
}

interface CommentsPageResponse {
  type: 'comments-page';
  data: Comment[];
  host: Patp;
  notebook: BookId;
  note: NoteId;
  startIndex: number;
  length: number;
}
