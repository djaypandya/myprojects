export type TaskStatus = 'todo' | 'doing' | 'done';

export type TimeBucket = 
  | 'today' 
  | 'today_extra' 
  | 'this_week' 
  | 'someday' 
  | 'inbox' 
  | 'tomorrow';

export interface Context {
  id: string;
  name: string;
  sort_order: number;
  theme: 'work' | 'home' | 'personal'; // Added for styling
}

export interface Task {
  id: string;
  context_id: string | null; // null for inbox items initially? Or maybe default to a context? Spec says Inbox items are unassigned.
  title: string;
  description?: string;
  status: TaskStatus;
  when: TimeBucket;
  is_week_priority: boolean;
  order_index: number;
  created_at: number;
  updated_at: number;
}

export interface AppState {
  contexts: Context[];
  tasks: Task[];
  activeContextId: string;
}
