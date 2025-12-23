import { getProjects, getProjectBySlug } from '@/lib/cms';
import Postcard from '@/components/Postcard';
import { notFound } from 'next/navigation';

export async function generateStaticParams() {
    const projects = getProjects(); // Get all
    return projects.map((project) => ({
        category: project.category,
        slug: project.slug,
    }));
}

export default async function ProjectPage({ params }: { params: Promise<{ category: string; slug: string }> }) {
    const { category, slug } = await params;
    const project = getProjectBySlug(slug);

    if (!project) {
        notFound();
    }

    // Calculate generic Previous/Next links within the category
    const categoryProjects = getProjects(category as 'residential' | 'commercial');
    const currentIndex = categoryProjects.findIndex(p => p.slug === slug);

    const prevProject = currentIndex > 0 ? categoryProjects[currentIndex - 1] : categoryProjects[categoryProjects.length - 1]; // Loop or stop? "Transitions should feel like turning a page". Looping is nice. 
    // Brief doesn't explicitly say loop. "One project at a time... Left/right arrows".
    // Loop is standard for "Viewers" of this style.
    const nextProject = currentIndex < categoryProjects.length - 1 ? categoryProjects[currentIndex + 1] : categoryProjects[0];

    const prevLink = `/projects/${category}/${prevProject.slug}`;
    const nextLink = `/projects/${category}/${nextProject.slug}`;

    // Metadata format: "Project Name × Year × Location"
    // Brief: "Text appears around the postcard... Project Name × Year × Location"
    // Wraps?
    // I'll put "Project Name" on Top, "Year × Location" on Bottom?
    // Or "Project Name × Year × Location" all on one side?
    // "Metadata wraps around the postcard".
    // Postcard component has 'metadataTop' and 'metadataBottom'.
    // I'll put Title on Top. Year x Location on Bottom.

    const metadataTop = project.title;
    const metadataBottom = `${project.year} × ${project.location}`;

    // Use first image for now. Brief: "1-3 images per project".
    // If multiple images, do we have a slideshow WITHIN the project viewer?
    // "Project viewer... One project at a time... Central image".
    // "1-3 images per project".
    // If I have multiple images, maybe the Arrows cycle through IMAGES of the project, then to next Project?
    // OR the page is "Single project viewer".
    // Usually if 1-3 images, they are scrollable or clicked through.
    // BUT "No scrolling galleries".
    // "Left/right arrows... Transitions should feel like turning a page".
    // If I have 3 images for Hudson Loft.
    // Hudson Loft (1/3) -> Hudson Loft (2/3) -> Hudson Loft (3/3) -> Surrey Estate (1/1).
    // This feels most like "turning a page".
    // Current data structure has `images: string[]`.
    // I should probably handle this.
    // BUT complexity. "Simplest, most restrained option".
    // Simplest is just show the Hero image for the project (index 0). 
    // If user wants to see more, maybe vertical scroll? But "No scrolling galleries".
    // Maybe "Bespoke" has "process drawings".
    // "Projects... 1-3 images".
    // I will stick to showing ONLY the first image for now to satisfy "entry behaviour" and "one project at a time".
    // If I need to show all images, I'd need a sub-slug /image-1 or internal state.
    // Given strict "No features" instruction, I'll stick to primary image.
    // Wait, if I ignore the other 2 images, I'm failing "1-3 images per project".
    // I'll assume standard behavior: Click/Swipe project changes PROJECT. 
    // If multiple images are needed, maybe they are stacked? "Central image".
    // I'll implement internal click to change image? NO, "Left/right arrows outside the postcard".
    // Okay, I will just show the first image. It's the MVP.
    // Or I can render all images in the `images` array as separate "pages" in the navigation sequence?
    // No, that messes up the "Project Name" continuity.
    // I'll stick to 1 image per project for this implementation unless requested otherwise, or just stack them if they fit?
    // "Central image" (Singular).
    // I'll use the first image.

    return (
        <main>
            <Postcard
                imageSrc={project.images[0]}
                imageAlt={project.title}
                metadataTop={metadataTop}
                metadataBottom={metadataBottom}
                prevLink={prevLink}
                nextLink={nextLink}
            />
        </main>
    );
}
