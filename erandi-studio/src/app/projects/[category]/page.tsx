import { getProjects } from '@/lib/cms';
import { redirect } from 'next/navigation';

export default async function CategoryPage({ params }: { params: Promise<{ category: string }> }) {
    const { category } = await params;

    if (category !== 'residential' && category !== 'commercial') {
        // Handle invalid category or redirect to projects generic?
        // Brief doesn't say. I'll redirect to home or 404.
        // redirect('/');
        // Or maybe just let it fail? Better to handle.
    }

    const projects = getProjects(category as 'residential' | 'commercial');

    if (projects.length > 0) {
        redirect(`/projects/${category}/${projects[0].slug}`);
    } else {
        // No projects? Show placeholder or empty state.
        return <div>No projects found in this category.</div>;
    }
}
